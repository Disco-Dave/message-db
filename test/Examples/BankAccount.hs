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


-- * Primitive types and functions used throughout


newtype Money = Money
  { fromMoney :: Double
  }
  deriving (Eq, Ord, Num, Generic)
  deriving (Show) via Double
instance Aeson.ToJSON Money
instance Aeson.FromJSON Money


newtype AccountMetadata = AccountMetadata
  { createdFrom :: Message.GlobalPosition
  }
  deriving (Eq, Ord, Generic)
  deriving (Show) via Message.GlobalPosition
instance Aeson.ToJSON AccountMetadata
instance Aeson.FromJSON AccountMetadata


newtype AccountId = AccountId
  { fromAccountId :: Text
  }
  deriving (Eq, Ord)
  deriving (Show) via Text


newAccountId :: IO AccountId
newAccountId = do
  uuid <- UUID.V4.nextRandom
  pure . AccountId $ UUID.toText uuid


commandCategory :: CategoryName
commandCategory =
  StreamName.category "bankAccount:command"


commandStream :: AccountId -> StreamName
commandStream =
  StreamName.addIdentity commandCategory . coerce


entityCategory :: CategoryName
entityCategory =
  StreamName.category "bankAccount"


entityStream :: AccountId -> StreamName
entityStream =
  StreamName.addIdentity entityCategory . coerce


minimumBalance :: Money
minimumBalance =
  5


-- * Events and commands. The commands are present tense, and events are past tense.


newtype Open = Open
  { initialDeposit :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Open
instance Aeson.FromJSON Open


newtype Opened = Opened
  { openedBalance :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Opened
instance Aeson.FromJSON Opened


data OpenRejectedReason
  = AccountIsAlreadyOpened
  | InitialDepositIsLessThan Money
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON OpenRejectedReason
instance Aeson.FromJSON OpenRejectedReason


newtype OpenRejected = OpenRejected
  { openRejectedReason :: Set OpenRejectedReason
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON OpenRejected
instance Aeson.FromJSON OpenRejected


data Close = Close
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Close
instance Aeson.FromJSON Close


data Closed = Closed
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Closed
instance Aeson.FromJSON Closed


data CloseRejectedReason
  = AccountIsAlreadyClosed
  | AccountBalanceIsNonZero
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON CloseRejectedReason
instance Aeson.FromJSON CloseRejectedReason


newtype CloseRejected = CloseRejected
  { closeRejectedReason :: Set CloseRejectedReason
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON CloseRejected
instance Aeson.FromJSON CloseRejected


newtype Deposit = Deposit
  { depositAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Deposit
instance Aeson.FromJSON Deposit


newtype Deposited = Deposited
  { depositedAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Deposited
instance Aeson.FromJSON Deposited


data DepositRejectedReason
  = DepositToClosedAccount
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON DepositRejectedReason
instance Aeson.FromJSON DepositRejectedReason


data DepositRejected = DepositRejected
  { rejectedDepositAmount :: Money
  , depositRejectedReason :: DepositRejectedReason
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON DepositRejected
instance Aeson.FromJSON DepositRejected


newtype Withdraw = Withdraw
  { withdrawAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Withdraw
instance Aeson.FromJSON Withdraw


newtype Withdrawn = Withdrawn
  { withdrawnAmount :: Money
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON Withdrawn
instance Aeson.FromJSON Withdrawn


data WithdrawRejectedReason
  = WithdrawFromClosedAccount
  | InsufficientFunds
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON WithdrawRejectedReason
instance Aeson.FromJSON WithdrawRejectedReason


data WithdrawRejected = WithdrawRejected
  { rejectedWithdrawAmount :: Money
  , withdrawRejectedReason :: WithdrawRejectedReason
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON WithdrawRejected
instance Aeson.FromJSON WithdrawRejected


-- * Projection stuff


data Overdraft = Overdraft
  { overdraftAmount :: Money
  , overdraftTime :: UTCTime
  }
  deriving (Show, Eq)


-- The final state
data BankAccount = BankAccount
  { balance :: Money
  , isOpened :: Bool
  , overdrafts :: [Overdraft]
  , commandsProcessed :: Set Message.GlobalPosition
  }
  deriving (Show, Eq)


initialAccount :: BankAccount
initialAccount =
  BankAccount
    { balance = 0
    , isOpened = False
    , overdrafts = []
    , commandsProcessed = Set.empty
    }


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


opened :: TypedMessage Opened Message.Metadata -> BankAccount -> BankAccount
opened TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { isOpened = True
    , balance = openedBalance payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


closed :: TypedMessage Closed Message.Metadata -> BankAccount -> BankAccount
closed TypedMessage{metadata} bankAccount =
  bankAccount
    { isOpened = False
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


deposited :: TypedMessage Deposited Message.Metadata -> BankAccount -> BankAccount
deposited TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount + depositedAmount payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


withdrawn :: TypedMessage Withdrawn Message.Metadata -> BankAccount -> BankAccount
withdrawn TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount - withdrawnAmount payload
    , commandsProcessed = addCommandPosition metadata (commandsProcessed bankAccount)
    }


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
        { initial = initialAccount
        , handlers = handlers
        }


-- * Subscription Stuff


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


fetch :: AccountId -> TestApp (Maybe (Projected BankAccount))
fetch accountId = do
  TestAppData{connectionPool} <- ask
  liftIO $
    Projection.fetch
      (Pool.withResource connectionPool)
      (Functions.FixedSize 100)
      (entityStream accountId)
      projection


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

  let bankAccount = maybe initialAccount Projection.state projectedAccount
      hasBeenRan = Set.member globalPosition $ commandsProcessed bankAccount

  unless hasBeenRan $ do
    let (eventType, event) =
          case processCommand payload bankAccount of
            Left failure -> (Message.typeOf @failure, Aeson.toJSON failure)
            Right success -> (Message.typeOf @success, Aeson.toJSON success)

        metadata = AccountMetadata globalPosition

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


subscribe :: TestApp Subscription
subscribe =
  withRunInIO $ \runInIO ->
    pure $
      (Subscription.subscribe commandCategory)
        { Subscription.failureStrategy = FailureStrategy $ \_ reason -> throwIO reason
        , Subscription.handlers =
            let attach eventType processCommand =
                  SubscriptionHandlers.attach eventType (runInIO . handleCommand processCommand)
             in SubscriptionHandlers.empty
                  & attach (Message.typeOf @Open) open
                  & attach (Message.typeOf @Close) close
                  & attach (Message.typeOf @Deposit) deposit
                  & attach (Message.typeOf @Withdraw) withdraw
        }
