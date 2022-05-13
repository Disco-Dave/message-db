module MessageDb.SubscriptionSpec (spec) where

import qualified Data.Pool as Pool
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import Test.Hspec
import qualified TestApp


spec :: Spec
spec =
  describe "Bank Account Example" . around (TestApp.withSubscriptions [BankAccount.subscribe]) $
    it "can open an account" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      let connectionPool = TestApp.connectionPool testAppData

          commandStream = BankAccount.commandStream accountId
          entityStream = BankAccount.entityStream accountId

      _ <- Pool.withResource connectionPool $ \connection ->
        Functions.writeMessage
          connection
          commandStream
          (Message.typeOf @BankAccount.Open)
          (BankAccount.Open 200)
          (Nothing :: Maybe ())
          Nothing

      _ <- Pool.withResource connectionPool $ \connection ->
        Functions.writeMessage
          connection
          commandStream
          (Message.typeOf @BankAccount.Open)
          (BankAccount.Open 202)
          (Nothing :: Maybe ())
          Nothing

      TestApp.runWith testAppData $
        TestApp.blockUntilStreamHas entityStream 2

      messages <- Pool.withResource connectionPool $ \connection ->
        Functions.getStreamMessages
          connection
          entityStream
          Nothing
          Nothing
          Nothing

      length messages `shouldBe` 2
