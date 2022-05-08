{-# LANGUAGE AllowAmbiguousTypes #-}

module Examples.BankAccount
  ( Money (..),
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
    projection,
    subscribe,
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
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Typeable (Proxy (Proxy), Typeable, typeRep)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projection (Projection))
import qualified MessageDb.Projection as Projection
import qualified MessageDb.Projection.Handlers as ProjectionHandlers
import MessageDb.StreamName (CategoryName)
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import MessageDb.TypedMessage (TypedMessage (TypedMessage))
import qualified MessageDb.TypedMessage as TypedMessage
import TestApp (TestApp, TestAppData (TestAppData, connectionPool))
import qualified TestApp
import UnliftIO (MonadUnliftIO (withRunInIO))


-- * Primitive types and functions used throughout


newtype Money = Money
  { fromMoney :: Double
  }
  deriving (Show, Eq, Ord, Num, Generic)
instance Aeson.ToJSON Money
instance Aeson.FromJSON Money


newtype AccountId = AccountId
  { fromAccountId :: UUID
  }
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON AccountId
instance Aeson.FromJSON AccountId


newtype AccountMetadata = AccountMetadata
  { createdFrom :: Message.GlobalPosition
  }
  deriving (Show, Eq, Ord, Generic)
instance Aeson.ToJSON AccountMetadata
instance Aeson.FromJSON AccountMetadata


messageType :: forall event. Typeable event => Message.MessageType
messageType =
  let eventName = Text.pack . show . typeRep $ Proxy @event
   in Message.MessageType eventName


commandCategory :: CategoryName
commandCategory =
  StreamName.category "bankAccount:command"


entityCategory :: CategoryName
entityCategory =
  StreamName.category "bankAccount"


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
  = DepositFromClosedAccount
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


handleOpened :: TypedMessage Opened AccountMetadata -> BankAccount -> BankAccount
handleOpened TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { isOpened = True
    , balance = openedBalance payload
    , commandsProcessed = Set.insert (createdFrom metadata) (commandsProcessed bankAccount)
    }


handleClosed :: TypedMessage Closed AccountMetadata -> BankAccount -> BankAccount
handleClosed TypedMessage{metadata} bankAccount =
  bankAccount
    { isOpened = False
    , commandsProcessed = Set.insert (createdFrom metadata) (commandsProcessed bankAccount)
    }


handleDeposited :: TypedMessage Deposited AccountMetadata -> BankAccount -> BankAccount
handleDeposited TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount + depositedAmount payload
    , commandsProcessed = Set.insert (createdFrom metadata) (commandsProcessed bankAccount)
    }


handleWithdrawn :: TypedMessage Withdrawn AccountMetadata -> BankAccount -> BankAccount
handleWithdrawn TypedMessage{payload, metadata} bankAccount =
  bankAccount
    { balance = balance bankAccount - withdrawnAmount payload
    , commandsProcessed = Set.insert (createdFrom metadata) (commandsProcessed bankAccount)
    }


handleWithdrawRejected :: TypedMessage WithdrawRejected (Maybe Aeson.Value) -> BankAccount -> BankAccount
handleWithdrawRejected TypedMessage{payload, createdAtTimestamp} bankAccount =
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
        }


projection :: Projection BankAccount
projection =
  let handlers =
        ProjectionHandlers.empty
          & ProjectionHandlers.attach (messageType @Opened) handleOpened
          & ProjectionHandlers.attach (messageType @Closed) handleClosed
          & ProjectionHandlers.attach (messageType @Deposited) handleDeposited
          & ProjectionHandlers.attach (messageType @Withdrawn) handleWithdrawn
          & ProjectionHandlers.attach (messageType @WithdrawRejected) handleWithdrawRejected
   in Projection
        { initial = initialAccount
        , handlers = handlers
        }


-- * Subscription Stuff


processOpen :: Maybe BankAccount -> Open -> Either OpenRejected Opened
processOpen maybeAccount payload =
  let accountIsOpen = maybe False isOpened maybeAccount
      errors =
        Set.fromList . catMaybes $
          [ if accountIsOpen
              then Just AccountIsAlreadyOpened
              else Nothing
          , if initialDeposit payload < minimumBalance
              then Just $ InitialDepositIsLessThan minimumBalance
              else Nothing
          ]
   in if Set.null errors
        then Right . Opened $ initialDeposit payload
        else Left $ OpenRejected errors


processClose :: Maybe BankAccount -> Close -> Either CloseRejected Closed
processClose maybeAccount _ =
  let accountIsOpen = maybe False isOpened maybeAccount
      currentBalance = maybe 0 balance maybeAccount
      errors =
        Set.fromList . catMaybes $
          [ if not accountIsOpen
              then Just AccountIsAlreadyClosed
              else Nothing
          , if currentBalance /= 0
              then Just AccountBalanceIsNonZero
              else Nothing
          ]
   in if Set.null errors
        then Right Closed
        else Left $ CloseRejected errors


processDeposit :: Maybe BankAccount -> Deposit -> Either DepositRejected Deposited
processDeposit maybeAccount payload =
  let accountIsOpen = maybe False isOpened maybeAccount
   in if accountIsOpen
        then Right . Deposited $ depositAmount payload
        else
          Left $
            DepositRejected
              { rejectedDepositAmount = depositAmount payload
              , depositRejectedReason = DepositFromClosedAccount
              }


processWithdraw :: Maybe BankAccount -> Withdraw -> Either WithdrawRejected Withdrawn
processWithdraw maybeAccount payload =
  let accountIsOpen = maybe False isOpened maybeAccount
      currentBalance = maybe 0 balance maybeAccount
      reject reason =
        Left
          WithdrawRejected
            { rejectedWithdrawAmount = withdrawAmount payload
            , withdrawRejectedReason = reason
            }
   in if accountIsOpen
        then
          if (currentBalance >= 0) && (withdrawAmount payload >= 0) && (withdrawAmount payload <= currentBalance)
            then Right . Withdrawn $ withdrawAmount payload
            else reject InsufficientFunds
        else reject WithdrawFromClosedAccount


handleCommand ::
  forall command failure success.
  ( Typeable failure
  , Aeson.ToJSON failure
  , Typeable success
  , Aeson.ToJSON success
  ) =>
  (Maybe BankAccount -> command -> Either failure success) ->
  TypedMessage command (Maybe Aeson.Value) ->
  TestApp ()
handleCommand processCommand TypedMessage{payload, globalPosition, streamName} = do
  Just identityName <- pure $ StreamName.identity streamName

  let targetStream = StreamName.addIdentity entityCategory identityName

  TestAppData{connectionPool} <- ask
  projectedAccount <-
    liftIO $
      Projection.fetch
        (Pool.withResource connectionPool)
        (Functions.FixedSize 100)
        targetStream
        projection

  let hasBeenRan =
        Set.member globalPosition $
          maybe Set.empty (commandsProcessed . Projection.state) projectedAccount

  unless hasBeenRan $ do
    let (eventType, event) =
          case processCommand (fmap Projection.state projectedAccount) payload of
            Left rejected -> (messageType @failure, Aeson.toJSON rejected)
            Right opened -> (messageType @success, Aeson.toJSON opened)

        metadata = AccountMetadata globalPosition
        expectedVersion = maybe 0 (Functions.ExpectedVersion . Projection.version) projectedAccount

    TestApp.withConnection $ \connection ->
      void . liftIO $
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
        { Subscription.handlers =
            let attach eventType processCommand =
                  SubscriptionHandlers.attach eventType (runInIO . handleCommand processCommand)
             in SubscriptionHandlers.empty
                  & attach (messageType @Open) processOpen
                  & attach (messageType @Close) processClose
                  & attach (messageType @Deposit) processDeposit
                  & attach (messageType @Withdraw) processWithdraw
        }
