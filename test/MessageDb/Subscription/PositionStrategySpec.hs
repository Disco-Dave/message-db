module MessageDb.Subscription.PositionStrategySpec (spec) where

import Control.Monad (replicateM_)
import Control.Monad.Reader (ask)
import qualified Data.Pool as Pool
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName)
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import Test.Hspec
import TestApp (TestApp, TestAppData (..))
import qualified TestApp


subscribe :: (TestAppData -> PositionStrategy) -> TestApp Subscription
subscribe positionStrategy = do
  subscription <- BankAccount.subscribe
  testAppData <- ask
  pure $
    subscription
      { Subscription.positionStrategy = positionStrategy testAppData
      , Subscription.messagesPerTick = 5
      }


positionStream :: StreamName
positionStream =
  "position-someUniqueName"


writeToStream :: TestAppData -> PositionStrategy
writeToStream TestAppData{connectionPool} =
  PositionStrategy.writeToStream
    (Pool.withResource connectionPool)
    5
    "position-someUniqueName"


spec :: Spec
spec =
  describe "writeToStream" $
    around (TestApp.withSubscriptions [subscribe writeToStream]) $
      it "saves position" $ \testAppData -> do
        accountId <- BankAccount.newAccountId

        let connectionPool = TestApp.connectionPool testAppData

        Pool.withResource connectionPool $ \connection ->
          replicateM_ 21 $
            Functions.writeMessage
              connection
              (BankAccount.commandStream accountId)
              (Message.typeOf @BankAccount.Open)
              (BankAccount.Open 40)
              (Nothing :: Maybe ())
              Nothing

        TestApp.runWith testAppData $
          TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 21

        messages <-
          Pool.withResource connectionPool $ \connection ->
            Functions.getStreamMessages
              connection
              positionStream
              Nothing
              Nothing
              Nothing

        abs (length messages - 4) `shouldSatisfy` (<= 1)
