module MessageDb.SubscriptionSpec (spec) where

import qualified Data.Pool as Pool
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Projection as Projection
import Test.Hspec
import qualified TestApp


spec :: Spec
spec =
  describe "Bank Account Example" . around (TestApp.withSubscriptions [BankAccount.subscribe]) $
    it "handles commands and can be projected" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      let connectionPool = TestApp.connectionPool testAppData
          entityStream = BankAccount.entityStream accountId

      TestApp.runWith testAppData $ do
        BankAccount.send accountId (BankAccount.Open 200)
        BankAccount.send accountId (BankAccount.Open 202)
        BankAccount.send accountId (BankAccount.Deposit 20)
        BankAccount.send accountId (BankAccount.Deposit 14)
        BankAccount.send accountId (BankAccount.Withdraw 100)
        TestApp.blockUntilStreamHas entityStream 5

      Just projectedAccount <-
        Projection.fetch
          (Pool.withResource connectionPool)
          (Functions.FixedSize 100)
          (BankAccount.entityStream accountId)
          BankAccount.projection

      let bankAccount = Projection.state projectedAccount
          commandsProcessed = BankAccount.commandsProcessed bankAccount

      length commandsProcessed `shouldBe` 5
