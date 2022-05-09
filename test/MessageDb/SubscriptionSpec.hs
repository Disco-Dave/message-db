module MessageDb.SubscriptionSpec (spec) where

import qualified Data.Pool as Pool
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import qualified MessageDb.StreamName as StreamName
import Test.Hspec
import qualified TestApp


newAccountId :: IO StreamName.IdentityName
newAccountId = do
  uuid <- UUID.V4.nextRandom
  pure . StreamName.IdentityName $ UUID.toText uuid


spec :: Spec
spec =
  describe "Bank Account Example" . around (TestApp.withSubscriptions [BankAccount.subscribe]) $
    fit "can open an account" $ \testAppData -> do
      accountId <- newAccountId

      let connectionPool = TestApp.connectionPool testAppData

          commandStream = StreamName.addIdentity BankAccount.commandCategory accountId
          entityStream = StreamName.addIdentity BankAccount.entityCategory accountId

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

      print messages

      length messages `shouldBe` 2
