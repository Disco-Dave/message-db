{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Examples.BankAccount
  ( Money (..)
  , AccountId (..)
  , AccountMetadata (..)
  , Open (..)
  , Opened (..)
  , OpenRejectedReason (..)
  , OpenRejected (..)
  , Close (..)
  , Closed (..)
  , CloseRejectedReason (..)
  , CloseRejected (..)
  , Deposit (..)
  , Deposited (..)
  , DepositRejectedReason (..)
  , DepositRejected (..)
  , Withdraw (..)
  , Withdrawn (..)
  , WithdrawRejectedReason (..)
  , WithdrawRejected (..)
  , Overdraft (..)
  , BankAccount (..)
  , newAccountId
  , commandCategory
  , commandStream
  , entityCategory
  , entityStream
  , projection
  , open
  , close
  , deposit
  , withdraw
  , subscribe
  , send
  , markAsProcessed
  , fetch
  )
where

import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (MonadReader (ask))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Maybe (catMaybes)
import qualified Data.Pool as Pool
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Data.Typeable (Typeable)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import GHC.Generics (Generic)
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message (..))
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projected, Projection (Projection, handlers))
import qualified MessageDb.Projection as Projection
import MessageDb.StreamName (Category, StreamName)
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.FailedMessage (FailedMessage (FailedMessage, failedReason))
import MessageDb.Subscription.FailureStrategy (FailureStrategy (..))
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


-- | The metadata of the message that is written to the entity stream.
-- For example when handling a command from bankAccount:command with a global position of 123,
-- then we write the resulting message to the entity stream with @createdFrom@ set to 123.
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
commandCategory :: Category
commandCategory =
  StreamName.categoryOfStream "bankAccount:command"


-- | Stream that you write commands for a specific bank account to.
commandStream :: AccountId -> StreamName
commandStream =
  StreamName.addIdentifierToCategory commandCategory . coerce


-- | Category of bank account events.
entityCategory :: Category
entityCategory =
  StreamName.categoryOfStream "bankAccount"


-- | The source of truth for a specific bank account. This is a stream of events from processing commands.
entityStream :: AccountId -> StreamName
entityStream =
  StreamName.addIdentifierToCategory entityCategory . coerce


-- | The minimum balance needed to keep an account open.
minimumBalance :: Money
minimumBalance =
  5


-- | An overdraft happens when there is a request to withdraw more money than what's in the account.
data Overdraft = Overdraft
  { overdraftAmount :: Money
  , overdraftTime :: UTCTime
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON Overdraft
instance Aeson.FromJSON Overdraft


-- Aggregation of all bank account events in the 'entityStream'.
data BankAccount = BankAccount
  { balance :: Money
  , isOpened :: Bool
  , overdrafts :: [Overdraft]
  , commandsProcessed :: Set Message.GlobalPosition
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON BankAccount
instance Aeson.FromJSON BankAccount


addCommandPosition :: Message.Metadata -> Set Message.GlobalPosition -> Set Message.GlobalPosition
addCommandPosition metadata positions =
  case Message.parseMetadata metadata of
    Left _ -> positions
    Right AccountMetadata{createdFrom} -> Set.insert createdFrom positions


markAsProcessed :: Message.Metadata -> BankAccount -> BankAccount
markAsProcessed metadata bankAccount =
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
opened :: Opened -> BankAccount -> BankAccount
opened payload bankAccount =
  bankAccount
    { isOpened = True
    , balance = openedBalance payload
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
closed :: Closed -> BankAccount -> BankAccount
closed _ bankAccount =
  bankAccount
    { isOpened = False
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
deposited :: Deposited -> BankAccount -> BankAccount
deposited payload bankAccount =
  bankAccount
    { balance = balance bankAccount + depositedAmount payload
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
  , rejectedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON WithdrawRejected
instance Aeson.FromJSON WithdrawRejected


-- | Handle the request to withdraw money.
withdraw :: Withdraw -> BankAccount -> TestApp (Either WithdrawRejected Withdrawn)
withdraw payload bankAccount = do
  let reject reason = do
        now <- liftIO Time.getCurrentTime
        pure . Left $
          WithdrawRejected
            { rejectedWithdrawAmount = withdrawAmount payload
            , withdrawRejectedReason = reason
            , rejectedAt = now
            }

  if isOpened bankAccount
    then
      if (balance bankAccount >= 0)
        && (withdrawAmount payload >= 0)
        && (withdrawAmount payload <= balance bankAccount)
        then pure . Right . Withdrawn $ withdrawAmount payload
        else reject InsufficientFunds
    else reject WithdrawFromClosedAccount


-- | Record the 'Withdrawn' event in our 'BankAccount' 'projection'.
withdrawn :: Withdrawn -> BankAccount -> BankAccount
withdrawn payload bankAccount =
  bankAccount
    { balance = balance bankAccount - withdrawnAmount payload
    }


-- | Record a 'WithdrawRejected' event in our 'BankAccount' 'projection'. This is where we find 'Overdraft's.
withdrawRejected :: WithdrawRejected -> BankAccount -> BankAccount
withdrawRejected payload bankAccount =
  let overdraft =
        Overdraft
          { overdraftAmount = rejectedWithdrawAmount payload
          , overdraftTime = rejectedAt payload
          }
   in bankAccount
        { overdrafts =
            if withdrawRejectedReason payload == InsufficientFunds
              then overdraft : overdrafts bankAccount
              else overdrafts bankAccount
        }


-- | Project all messages in the 'entityStream' and aggregate the state into a 'BankAccount'.
projection :: Projection BankAccount
projection =
  let markAsProcessedHandler =
        Handlers.projectionHandler @Message.Payload $ \_ _ -> markAsProcessed
      toHandler f =
        markAsProcessedHandler <> (Handlers.projectionHandler @_ @Message.Metadata $ \_ payload _ -> f payload)
      handlers =
        Handlers.listToHandlers
          [ (Message.messageTypeOf @Opened, toHandler opened)
          , (Message.messageTypeOf @OpenRejected, markAsProcessedHandler)
          , (Message.messageTypeOf @Closed, toHandler closed)
          , (Message.messageTypeOf @CloseRejected, markAsProcessedHandler)
          , (Message.messageTypeOf @Deposited, toHandler deposited)
          , (Message.messageTypeOf @DepositRejected, markAsProcessedHandler)
          , (Message.messageTypeOf @Withdrawn, toHandler withdrawn)
          , (Message.messageTypeOf @WithdrawRejected, toHandler withdrawRejected)
          ]
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
      (Message.messageTypeOf @command)
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
  , Aeson.FromJSON command
  ) =>
  (forall a. TestApp a -> IO a) ->
  (command -> BankAccount -> TestApp (Either failure success)) ->
  Handlers.SubscriptionHandler
handleCommand runInIO processCommand =
  Handlers.subscriptionHandler @_ @Message.Metadata $ \Message{messageStream, messageGlobalPosition} command _ -> do
    Just accountId <- pure . coerce $ StreamName.identifierOfStream messageStream

    let targetStream = entityStream accountId

    projectedAccount <- runInIO $ fetch accountId

    let bankAccount = maybe (Projection.initial projection) Projection.state projectedAccount
        hasBeenRan = Set.member messageGlobalPosition $ commandsProcessed bankAccount

    -- This is where the `AccountMetadata' comes in handy. Since we recorded the global positions of commands that we already
    -- processed we can implement idempotence by checking if we've seen this global position before.
    unless hasBeenRan $ do
      result <- runInIO $ processCommand command bankAccount

      let (eventType, event) =
            case result of
              Left failure -> (Message.messageTypeOf @failure, Aeson.toJSON failure)
              Right success -> (Message.messageTypeOf @success, Aeson.toJSON success)

          -- Here is we we construct the metadata that will be useful later for idempotence checks.
          metadata = AccountMetadata messageGlobalPosition

          -- The expected version is used for optimistic concurrency and ensures that this event
          -- is the result of a command on the most up to date version of the stream.
          expectedVersion =
            Functions.ExpectedVersion $
              maybe Functions.DoesNotExist Projection.version projectedAccount

      runInIO . TestApp.withConnection $ \connection ->
        liftIO . void $
          Functions.writeMessage
            connection
            targetStream
            eventType
            event
            (Just metadata)
            (Just expectedVersion)


-- | Subscribe to the 'BankAccount's command stream. You'd use 'Subscription.start' to start this subscription.
-- 'subscribe' alongside 'handleCommand' demostrate how it's possible to construct a subscription that uses a monad other than IO.
subscribe :: TestApp Subscription
subscribe =
  withRunInIO $ \runInIO -> do
    pure $
      (Subscription.subscribe commandCategory)
        { Subscription.failureStrategy =
            -- This is a bad idea for production code. This is here because I want to crash the test if an exception occurred.
            -- Look at 'MessageDb.Subscription.FailureStrategy' for different ways to handle this.
            FailureStrategy $ \FailedMessage{failedReason} -> throwIO failedReason
        , Subscription.handlers =
            Handlers.listToHandlers
              [ (Message.messageTypeOf @Open, handleCommand runInIO (\m p -> pure $ open m p))
              , (Message.messageTypeOf @Close, handleCommand runInIO (\m p -> pure $ close m p))
              , (Message.messageTypeOf @Deposit, handleCommand runInIO (\m p -> pure $ deposit m p))
              , (Message.messageTypeOf @Withdraw, handleCommand runInIO withdraw)
              ]
        }
