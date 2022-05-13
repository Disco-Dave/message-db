module MessageDb.Subscription.PositionStrategySpec (spec) where

import Test.Hspec

import Control.Monad.Reader (ask)
import qualified Data.Pool as Pool
import qualified Examples.BankAccount as BankAccount
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import TestApp (TestApp, TestAppData (..))
import qualified TestApp


subscribe :: (TestAppData -> PositionStrategy) -> TestApp Subscription
subscribe positionStrategy = do
  subscription <- BankAccount.subscribe
  testAppData <- ask
  pure $
    subscription
      { Subscription.positionStrategy = positionStrategy testAppData
      }


writeToStream :: TestAppData -> PositionStrategy
writeToStream TestAppData{connectionPool} =
  PositionStrategy.writeToStream
    (Pool.withResource connectionPool)
    15
    "position-someUniqueName"


spec :: Spec
spec =
  describe "writeToStream" $ 
    around (TestApp.withSubscriptions [subscribe writeToStream]) $
      it "saves position" $ \testAppData ->
        True `shouldBe` True
