module MessageDb.Subscription.FailureStrategySpec (spec) where

import Control.Monad (void, when)
import Control.Monad.Reader (ask)
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Function ((&))
import qualified Data.Pool as Pool
import qualified Examples.BankAccount as BankAccount
import GHC.Generics (Generic)
import qualified MessageDb
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import qualified MessageDb.Subscription.FailedMessage as FailedMessage
import qualified MessageDb.Subscription.FailureStrategy as FailureStrategy
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import MessageDb.TypedMessage (TypedMessage)
import qualified MessageDb.TypedMessage as TypedMessage
import Test.Hspec
import TestApp (TestApp)
import qualified TestApp
import UnliftIO.Exception (throwString)


newtype ComputeInterest = ComputeInterest
  { shouldFail :: Bool
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON ComputeInterest
instance Aeson.FromJSON ComputeInterest


newtype InterestComputed = InterestComputed
  { rate :: Double
  }
  deriving (Show, Eq, Generic)
instance Aeson.ToJSON InterestComputed
instance Aeson.FromJSON InterestComputed


computeInterest :: Bool -> Functions.WithConnection -> TypedMessage ComputeInterest Message.Metadata -> IO ()
computeInterest canFail withConnection message = do
  let ComputeInterest{shouldFail} = TypedMessage.payload message

  when (shouldFail && canFail) $
    throwString "Something bad happened"

  Just identity <-
    pure . MessageDb.identityOfStream $ TypedMessage.streamName message

  void . withConnection $ \connection ->
    Functions.writeMessage
      connection
      (BankAccount.entityStream $ coerce identity)
      (Message.typeOf @InterestComputed)
      (InterestComputed 0.01)
      (Just . BankAccount.AccountMetadata $ TypedMessage.globalPosition message)
      Nothing


failureCategory :: MessageDb.CategoryName
failureCategory =
  MessageDb.categoryName "failures"


failureStream :: BankAccount.AccountId -> StreamName.StreamName
failureStream =
  MessageDb.addIdentityToCategory failureCategory . coerce


subscribeCommands :: TestApp Subscription
subscribeCommands = do
  subscription <- BankAccount.subscribe
  testAppData <- ask

  let connectionPool = TestApp.connectionPool testAppData
      handlers =
        Subscription.handlers subscription
          & SubscriptionHandlers.attach
            (Message.typeOf @ComputeInterest)
            (computeInterest True (Pool.withResource connectionPool))

  pure $
    subscription
      { Subscription.handlers = handlers
      , Subscription.failureStrategy =
          FailureStrategy.writeAllToCategory
            (Pool.withResource connectionPool)
            failureCategory
      }


subscribeFailures :: TestApp Subscription
subscribeFailures = do
  subscription <- BankAccount.subscribe
  testAppData <- ask

  let connectionPool = TestApp.connectionPool testAppData
      handlers =
        Subscription.handlers subscription
          & SubscriptionHandlers.attach
            (Message.typeOf @ComputeInterest)
            (computeInterest False (Pool.withResource connectionPool))
          & FailedMessage.handleFailures

  pure $
    subscription
      { Subscription.failureStrategy = FailureStrategy.ignoreFailures
      , Subscription.categoryName = MessageDb.categoryName "failures"
      , Subscription.handlers = handlers
      }


spec :: Spec
spec = do
  let subscriptions =
        [ subscribeFailures
        , subscribeCommands
        ]

  around (TestApp.withSubscriptions subscriptions) $
    describe "writeToCategory" $
      it "writes failures to category" $ \testAppData -> do
        accountId <- BankAccount.newAccountId

        TestApp.runWith testAppData $ do
          BankAccount.send accountId $ BankAccount.Open 100
          BankAccount.send accountId $ BankAccount.Deposit 20
          BankAccount.send accountId $ ComputeInterest True
          TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 3

        let connectionPool = TestApp.connectionPool testAppData

        failedMessages <-
          Pool.withResource connectionPool $ \connection ->
            Functions.getStreamMessages
              connection
              (failureStream accountId)
              Nothing
              Nothing
              Nothing

        entityMessages <-
          Pool.withResource connectionPool $ \connection ->
            Functions.getStreamMessages
              connection
              (BankAccount.entityStream accountId)
              Nothing
              Nothing
              Nothing

        length failedMessages `shouldBe` 1
        length entityMessages `shouldBe` 3
