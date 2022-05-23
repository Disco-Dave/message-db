{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Examples.BankAccount
  ( Money (..),
    AccountId (..),
    AccountMetadata (..),
    Open (..),
    Opened (..),
    OpenRejectedReason (..),
    OpenRejected (..),
    Close (..),
    Closed (..),
    CloseRejectedReason (..),
    CloseRejected (..),
    Deposit (..),
    Deposited (..),
    DepositRejectedReason (..),
    DepositRejected (..),
    Withdraw (..),
    Withdrawn (..),
    WithdrawRejectedReason (..),
    WithdrawRejected (..),
    Overdraft (..),
    BankAccount (..),
    newAccountId,
    commandCategory,
    commandStream,
    entityCategory,
    entityStream,
    projection,
    open,
    close,
    deposit,
    withdraw,
    subscribe,
    send,
    markAsProcessed,
    fetch,
  )
where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Maybe (catMaybes)
import qualified Data.Pool as Pool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import GHC.Generics (Generic)
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projected, Projection (Projection, handlers))
import qualified MessageDb.Projection as Projection
import qualified MessageDb.Projection.Handlers as ProjectionHandlers
import MessageDb.StreamName (CategoryName, StreamName)
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.FailureStrategy (FailureStrategy (..))
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import MessageDb.TypedMessage (TypedMessage (TypedMessage))
import qualified MessageDb.TypedMessage as TypedMessage
import TestApp (TestApp, TestAppData (TestAppData, connectionPool))
import qualified TestApp
import UnliftIO (MonadUnliftIO (withRunInIO), throwIO)


-- | A unit of measure representing money.
newtype Money = Money
  { fromMoney :: Double
  }
  deriving (Eq, Ord, Num, Generic)
  deriving (Show) via Double


instance Aeson.ToJSON Money
instance Aeson.FromJSON Money


{- | The metadata of the message that is written to the entity stream.
 For example when handling a command from bankAccount:command with a global position of 123,
 then we write the resulting message to the entity stream with @createdFrom@ set to 123.
-}
newtype AccountMetadata = AccountMetadata
  { createdFrom :: Message.GlobalPosition
  }
  deriving (Eq, Ord, Generic)
  deriving (Show) via Message.GlobalPosition


instance Aeson.ToJSON AccountMetadata
instance Aeson.FromJSON AccountMetadata


-- | A unique id for a bank account.
newtype AccountId = AccountId
  { fromAccountId :: Text
  }
  deriving (Eq, Ord)
  deriving (Show) via Text


-- | Create a new unique account id.
newAccountId :: IO AccountId
newAccountId = do
  uuid <- UUID.V4.nextRandom
  pure . AccountId $ UUID.toText uuid


-- | Category of bank account commands.
commandCategory :: CategoryName
commandCategory =
  StreamName.category "bankAccount:command"


-- | Stream that you write commands for a specific bank account to.
commandStream :: AccountId -> StreamName
commandStream =
  StreamName.addIdentity commandCategory . coerce


-- | Category of bank account events.
entityCategory :: CategoryName
entityCategory =
  StreamName.category "bankAccount"


-- | The source of truth for a specific bank account. This is a stream of events from processing commands.
entityStream :: AccountId -> StreamName
entityStream =
  StreamName.addIdentity entityCategory . coerce


-- | The minimum balance needed to keep an account open.
minimumBalance :: Money
minimumBalance =
  5


-- | An overdraft happens when there is a request to withdraw more money than what's in the account.
data Overdraft = Overdraft
  { overdraftAmount :: Money
  , overdraftTime :: UTCTime
  }
  deriving (Show, Eq)


-- Aggregation of all bank account events in the 'entityStream'.
data BankAccount = BankAccount
  { balance :: Money
  , isOpened :: Bool
  , overdrafts :: [Overdraft]
  , commandsProcessed :: Set Message.GlobalPosition
  }
  deriving (Show, Eq)


addCommandPosition :: Message.Metadata -> Set Message.GlobalPosition -> Set Message.GlobalPosition
addCommandPosition metadata positions =
  case Message.parseMetadata metadata of
    Left _ -> positions
    Right AccountMetadata{createdFrom} -> Set.insert createdFrom positions


markAsProcessed :: TypedMessage payload Message.Metadata -> BankAccount -> BankAccount
markAsProcessed TypedMessage{metadata} bankAccount =
  bankAccount
    { commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


-- | Request to open a bank account with an initial deposit.
newtype Open = Open
  { initialDeposit :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Open
instance Aeson.FromJSON Open


-- | The event from successfully opening an account
newtype Opened = Opened
  { openedBalance :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Opened
instance Aeson.FromJSON Opened


-- | Reason why the 'Open' command was rejected.
data OpenRejectedReason
  = AccountIsAlreadyOpened
  | InitialDepositIsLessThan Money
  deriving (Show, Eq, Ord, Generic)


instance Aeson.ToJSON OpenRejectedReason
instance Aeson.FromJSON OpenRejectedReason


-- | The event from failing to open an account.
newtype OpenRejected = OpenRejected
  { openRejectedReason :: Set OpenRejectedReason
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON OpenRejected
instance Aeson.FromJSON OpenRejected


-- | Handle an 'Open' account command.
open :: Open -> BankAccount -> Either OpenRejected Opened
open payload bankAccount =
  let errors =
        Set.fromList . catMaybes $
          [ if isOpened bankAccount
              then Just AccountIsAlreadyOpened
              else Nothing
          , if initialDeposit payload < minimumBalance
              then Just $ InitialDepositIsLessThan minimumBalance
              else Nothing
          ]
   in if Set.null errors
        then Right . Opened $ initialDeposit payload
        else Left $ OpenRejected errors


-- | Record the 'Opened' event in our 'BankAccount' 'projection'.
opened :: TypedMessage Opened Message.Metadata -> BankAccount -> BankAccount
opened TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { isOpened = True
    , balance = openedBalance payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


-- | Request to close a bank account.
data Close = Close
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Close
instance Aeson.FromJSON Close


-- | The event from successfully closing an account.
data Closed = Closed
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Closed
instance Aeson.FromJSON Closed


-- | The reason why the 'Close' command failed.
data CloseRejectedReason
  = AccountIsAlreadyClosed
  | AccountBalanceIsNonZero
  deriving (Show, Eq, Ord, Generic)


instance Aeson.ToJSON CloseRejectedReason
instance Aeson.FromJSON CloseRejectedReason


-- | The event from failing to close an account.
newtype CloseRejected = CloseRejected
  { closeRejectedReason :: Set CloseRejectedReason
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON CloseRejected
instance Aeson.FromJSON CloseRejected


-- | Handle the command to close a bank account.
close :: Close -> BankAccount -> Either CloseRejected Closed
close _ bankAccount =
  let errors =
        Set.fromList . catMaybes $
          [ if not (isOpened bankAccount)
              then Just AccountIsAlreadyClosed
              else Nothing
          , if balance bankAccount /= 0
              then Just AccountBalanceIsNonZero
              else Nothing
          ]
   in if Set.null errors
        then Right Closed
        else Left $ CloseRejected errors


-- | Record the Closed event in our 'BankAccount' 'projection'.
closed :: TypedMessage Closed Message.Metadata -> BankAccount -> BankAccount
closed TypedMessage{metadata} bankAccount =
  bankAccount
    { isOpened = False
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


-- | Request to deposit money into the bank account.
newtype Deposit = Deposit
  { depositAmount :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Deposit
instance Aeson.FromJSON Deposit


-- | The event from successfully depositing money.
newtype Deposited = Deposited
  { depositedAmount :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Deposited
instance Aeson.FromJSON Deposited


-- | The reason why a 'Deposit' command was rejected.
data DepositRejectedReason
  = DepositToClosedAccount
  deriving (Show, Eq, Ord, Generic)


instance Aeson.ToJSON DepositRejectedReason
instance Aeson.FromJSON DepositRejectedReason


-- | The event from failing to deposit money.
data DepositRejected = DepositRejected
  { rejectedDepositAmount :: Money
  , depositRejectedReason :: DepositRejectedReason
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON DepositRejected
instance Aeson.FromJSON DepositRejected


-- | Handle the command to deposit money into the bank account.
deposit :: Deposit -> BankAccount -> Either DepositRejected Deposited
deposit payload bankAccount =
  if isOpened bankAccount
    then Right . Deposited $ depositAmount payload
    else
      Left $
        DepositRejected
          { rejectedDepositAmount = depositAmount payload
          , depositRejectedReason = DepositToClosedAccount
          }


-- | Record 'Deposited' event in our 'BankAccount' 'projection'.
deposited :: TypedMessage Deposited Message.Metadata -> BankAccount -> BankAccount
deposited TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount + depositedAmount payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


-- | Request to withdraw money from a bank account.
newtype Withdraw = Withdraw
  { withdrawAmount :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Withdraw
instance Aeson.FromJSON Withdraw


-- | The event from successfully withdrawing money.
newtype Withdrawn = Withdrawn
  { withdrawnAmount :: Money
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Withdrawn
instance Aeson.FromJSON Withdrawn


-- | The reason why a 'Withdraw' command failed.
data WithdrawRejectedReason
  = WithdrawFromClosedAccount
  | InsufficientFunds
  deriving (Show, Eq, Ord, Generic)


instance Aeson.ToJSON WithdrawRejectedReason
instance Aeson.FromJSON WithdrawRejectedReason


-- | The event from failing to withdraw money.
data WithdrawRejected = WithdrawRejected
  { rejectedWithdrawAmount :: Money
  , withdrawRejectedReason :: WithdrawRejectedReason
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON WithdrawRejected
instance Aeson.FromJSON WithdrawRejected


-- | Handle the request to withdraw money.
withdraw :: Withdraw -> BankAccount -> Either WithdrawRejected Withdrawn
withdraw payload bankAccount =
  let reject reason =
        Left
          WithdrawRejected
            { rejectedWithdrawAmount = withdrawAmount payload
            , withdrawRejectedReason = reason
            }
   in if isOpened bankAccount
        then
          if (balance bankAccount >= 0)
            && (withdrawAmount payload >= 0)
            && (withdrawAmount payload <= balance bankAccount)
            then Right . Withdrawn $ withdrawAmount payload
            else reject InsufficientFunds
        else reject WithdrawFromClosedAccount


-- | Record the 'Withdrawn' event in our 'BankAccount' 'projection'.
withdrawn :: TypedMessage Withdrawn Message.Metadata -> BankAccount -> BankAccount
withdrawn TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount - withdrawnAmount payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


-- | Record a 'WithdrawRejected' event in our 'BankAccount' 'projection'. This is where we find 'Overdraft's.
withdrawRejected :: TypedMessage WithdrawRejected Message.Metadata -> BankAccount -> BankAccount
withdrawRejected TypedMessage{payload, createdAtTimestamp, metadata} bankAccount =
  let overdraft =
        Overdraft
          { overdraftAmount = rejectedWithdrawAmount payload
          , overdraftTime = coerce createdAtTimestamp
          }
   in bankAccount
        { overdrafts =
            if withdrawRejectedReason payload == InsufficientFunds
              then overdraft : overdrafts bankAccount
              else overdrafts bankAccount
        , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
        }


-- | Project all messages in the 'entityStream' and aggregate the state into a 'BankAccount'.
projection :: Projection BankAccount
projection =
  let handlers =
        ProjectionHandlers.empty
          & ProjectionHandlers.attach (Message.typeOf @Opened) opened
          & ProjectionHandlers.attach @Message.Payload (Message.typeOf @OpenRejected) markAsProcessed
          & ProjectionHandlers.attach (Message.typeOf @Closed) closed
          & ProjectionHandlers.attach @Message.Payload (Message.typeOf @CloseRejected) markAsProcessed
          & ProjectionHandlers.attach (Message.typeOf @Deposited) deposited
          & ProjectionHandlers.attach @Message.Payload (Message.typeOf @DepositRejected) markAsProcessed
          & ProjectionHandlers.attach (Message.typeOf @Withdrawn) withdrawn
          & ProjectionHandlers.attach (Message.typeOf @WithdrawRejected) withdrawRejected
   in Projection
        { handlers = handlers
        , initial =
            BankAccount
              { balance = 0
              , isOpened = False
              , overdrafts = []
              , commandsProcessed = Set.empty
              }
        }


-- | Fetch the state of a bank account from Message-DB by it's 'AccountId'.
fetch :: AccountId -> TestApp (Maybe (Projected BankAccount))
fetch accountId = do
  TestAppData{connectionPool} <- ask
  liftIO $
    Projection.fetch
      (Pool.withResource connectionPool)
      (Functions.FixedSize 100)
      (entityStream accountId)
      projection


-- | Send a command to be asynchronously processed for a 'BankAccount'.
send :: forall command. (Aeson.ToJSON command, Typeable command) => AccountId -> command -> TestApp ()
send accountId command = do
  TestAppData{connectionPool} <- ask

  liftIO . void . Pool.withResource connectionPool $ \connection ->
    Functions.writeMessage
      connection
      (commandStream accountId)
      (Message.typeOf @command)
      command
      (Just Message.nullMetadata)
      Nothing


-- | Function that helps convert our pure functions, 'command -> BankAccount -> Either failure success', into a subscription command handler.
handleCommand ::
  forall command failure success.
  ( Typeable failure
  , Aeson.ToJSON failure
  , Typeable success
  , Aeson.ToJSON success
  ) =>
  (command -> BankAccount -> Either failure success) ->
  TypedMessage command Message.Metadata ->
  TestApp ()
handleCommand processCommand TypedMessage{payload, globalPosition, streamName} = do
  Just accountId <- pure . coerce $ StreamName.identity streamName

  let targetStream = entityStream accountId

  projectedAccount <- fetch accountId

  let bankAccount = maybe (Projection.initial projection) Projection.state projectedAccount
      hasBeenRan = Set.member globalPosition $ commandsProcessed bankAccount

  -- This is where the `AccountMetadata' comes in handy. Since we recorded the global positions of commands that we already
  -- processed we can implement idempotence by checking if we've seen this global position before.
  unless hasBeenRan $ do
    let (eventType, event) =
          case processCommand payload bankAccount of
            Left failure -> (Message.typeOf @failure, Aeson.toJSON failure)
            Right success -> (Message.typeOf @success, Aeson.toJSON success)

        -- Here is we we construct the metadata that will be useful later for idempotence checks.
        metadata = AccountMetadata globalPosition

        -- The expected version is used for optimistic concurrency and ensures that this event
        -- is the result of a command on the most up to date version of the stream.
        expectedVersion =
          Functions.ExpectedVersion $
            maybe Functions.DoesNotExist Projection.version projectedAccount

    TestApp.withConnection $ \connection ->
      liftIO . void $
        Functions.writeMessage
          connection
          targetStream
          eventType
          event
          (Just metadata)
          (Just expectedVersion)


-- | Subscribe to the 'BankAccount's command stream. You'd use 'Subscription.start' to start this subscription.
subscribe :: TestApp Subscription
subscribe =
  withRunInIO $ \runInIO ->
    pure $
      (Subscription.subscribe commandCategory)
        { Subscription.failureStrategy =
            -- This is a bad idea for production code. This is here because I want to crash the test if an exception occurred.
            -- Look at 'MessageDb.Subscription.FailureStrategy' for different ways to handle this.
            FailureStrategy $ \_ reason -> throwIO reason
        , Subscription.handlers =
            let attach eventType processCommand =
                  SubscriptionHandlers.attach eventType (runInIO . handleCommand processCommand)
             in SubscriptionHandlers.empty
                  & attach (Message.typeOf @Open) open
                  & attach (Message.typeOf @Close) close
                  & attach (Message.typeOf @Deposit) deposit
                  & attach (Message.typeOf @Withdraw) withdraw
        }
