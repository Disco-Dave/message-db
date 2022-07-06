module MessageDb.Subscription.PositionStrategySpec (spec) where

import Control.Concurrent (threadDelay)
import qualified Control.Concurrent.MVar as MVar
import Control.Monad (replicateM_)
import Control.Monad.Reader (ask)
import Data.Coerce (coerce)
import qualified Data.Pool as Pool
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Projection as Projection
import MessageDb.StreamName (StreamName)
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import Test.Hspec
import TestApp (TestApp, TestAppData (..))
import qualified TestApp
import qualified UnliftIO.Async as Async


subscribe :: (TestAppData -> PositionStrategy) -> TestApp Subscription
subscribe positionStrategy = do
  subscription <- BankAccount.subscribe
  testAppData <- ask
  pure $
    subscription
      { Subscription.positionStrategy = positionStrategy testAppData
      , Subscription.batchSize = 5
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
  describe "writeToStream" $ do
    around (TestApp.withSubscriptions [subscribe writeToStream]) $
      it "saves position" $ \testAppData -> do
        accountId <- BankAccount.newAccountId

        let connectionPool = TestApp.connectionPool testAppData

            numberOfMessages :: Num a => a
            numberOfMessages = 21

        TestApp.runWith testAppData . replicateM_ numberOfMessages $
          BankAccount.send accountId (BankAccount.Open 40)

        TestApp.runWith testAppData $
          TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) numberOfMessages

        messages <-
          Pool.withResource connectionPool $ \connection ->
            Functions.getStreamMessages
              connection
              positionStream
              Nothing
              Nothing
              Nothing

        abs (length messages - 4) `shouldSatisfy` (<= 1)

    around TestApp.withTestAppData $
      it "restores position" $ \testAppData -> do
        accountId <- BankAccount.newAccountId

        let connectionPool = TestApp.connectionPool testAppData
            numberOfMessages = 20

        TestApp.runWith testAppData . replicateM_ numberOfMessages $
          BankAccount.send accountId (BankAccount.Open 40)

        subscription <-
          TestApp.runWith testAppData $
            subscribe writeToStream

        let start = Subscription.start (Pool.withResource connectionPool)

        Async.withAsync (start subscription) $ \_ ->
          let check = do
                threadDelay 100_000

                messages <- Pool.withResource connectionPool $ \connection ->
                  Functions.getStreamMessages
                    connection
                    (BankAccount.entityStream accountId)
                    Nothing
                    Nothing
                    Nothing

                if length messages >= numberOfMessages
                  then pure ()
                  else check
           in check

        TestApp.runWith testAppData $
          BankAccount.send accountId (BankAccount.Open 40)

        Just projectedAccount <-
          Projection.fetch
            (Pool.withResource connectionPool)
            Functions.Unlimited
            (BankAccount.entityStream accountId)
            BankAccount.projection

        restoredVar <- MVar.newEmptyMVar

        let modifiedStrategy =
              let modifiedSave lastPositionSaved currentPosition = do
                    MVar.putMVar restoredVar (lastPositionSaved, currentPosition)
                    pure Nothing
               in (Subscription.positionStrategy subscription)
                    { PositionStrategy.save = modifiedSave
                    }

            modifiedSubscription =
              subscription
                { Subscription.positionStrategy = modifiedStrategy
                }

            expectedPositionSaved =
              let positions = BankAccount.commandsProcessed $ Projection.state projectedAccount
               in maximum positions

        (actualPositionSaved, actualCurrentPosition) <-
          Async.withAsync (start modifiedSubscription) $ \_ ->
            coerce $ MVar.takeMVar restoredVar

        actualPositionSaved `shouldSatisfy` (< actualCurrentPosition)
        actualPositionSaved `shouldBe` expectedPositionSaved
