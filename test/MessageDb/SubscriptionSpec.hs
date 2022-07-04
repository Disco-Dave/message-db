module MessageDb.SubscriptionSpec (spec) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader.Class (MonadReader (ask))
import Data.Function ((&))
import qualified Data.Pool as Pool
import qualified Data.Set as Set
import Examples.BankAccount (BankAccount)
import qualified Examples.BankAccount as BankAccount
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (ProjectionHandlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message (..))
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projected)
import qualified MessageDb.Projection as Projection
import Test.Hspec (Spec, around, describe, it, shouldBe)
import TestApp (TestApp, TestAppData (TestAppData))
import qualified TestApp


fetch :: ProjectionHandlers BankAccount -> BankAccount.AccountId -> TestApp (Maybe (Projected BankAccount))
fetch handlers accountId = do
  TestAppData{connectionPool} <- ask
  liftIO $
    Projection.fetch
      (Pool.withResource connectionPool)
      (Functions.FixedSize 100)
      (BankAccount.entityStream accountId)
      ( BankAccount.projection
          { Projection.handlers = handlers
          }
      )


fetchWithNoHandlers :: BankAccount.AccountId -> TestApp (Maybe (Projected BankAccount))
fetchWithNoHandlers =
  fetch mempty


fetchWithConversionErrors :: BankAccount.AccountId -> TestApp (Maybe (Projected BankAccount))
fetchWithConversionErrors =
  let handle :: Message -> Bool -> Bool -> BankAccount -> BankAccount
      handle _ _ _ bankAccount = bankAccount

      handlers =
        Handlers.emptyHandlers
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.Opened) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.OpenRejected) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.Closed) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.CloseRejected) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.Deposited) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.DepositRejected) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.Withdrawn) handle
          & Handlers.addProjectionHandler (Message.messageTypeOf @BankAccount.WithdrawRejected) handle
   in fetch handlers


spec :: Spec
spec =
  describe "Bank Account Example" . around (TestApp.withSubscriptions [BankAccount.subscribe]) $ do
    it "fetch returns nothing when a stream does not exist" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      projected <- TestApp.runWith testAppData $ BankAccount.fetch accountId

      projected `shouldBe` Nothing

    it "handles commands and can be projected" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      Just projectedAccount <-
        TestApp.runWith testAppData $ do
          BankAccount.send accountId (BankAccount.Open 200)
          BankAccount.send accountId (BankAccount.Open 202)
          BankAccount.send accountId (BankAccount.Deposit 20)
          BankAccount.send accountId (BankAccount.Deposit 14)
          BankAccount.send accountId (BankAccount.Withdraw 100)
          TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 5
          BankAccount.fetch accountId

      let bankAccount = Projection.state projectedAccount
          commandsProcessed = BankAccount.commandsProcessed bankAccount

      length commandsProcessed `shouldBe` 5
      BankAccount.balance bankAccount `shouldBe` 134
      BankAccount.isOpened bankAccount `shouldBe` True
      BankAccount.overdrafts bankAccount `shouldBe` []
      Set.size (BankAccount.commandsProcessed bankAccount) `shouldBe` 5
      Projection.version projectedAccount `shouldBe` Functions.DoesExist 4
      Projection.versionIncludingUnprocessed projectedAccount `shouldBe` Functions.DoesExist 4
      Projection.unprocessed projectedAccount `shouldBe` []

    it "missing handler errors are reported correctly" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      TestApp.runWith testAppData $ do
        BankAccount.send accountId (BankAccount.Open 200)
        BankAccount.send accountId (BankAccount.Open 202)
        BankAccount.send accountId (BankAccount.Deposit 20)
        BankAccount.send accountId (BankAccount.Deposit 14)
        BankAccount.send accountId (BankAccount.Withdraw 100)
        TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 5

      Just projectedAccount <-
        TestApp.runWith testAppData $ fetchWithNoHandlers accountId

      let unprocessed = Projection.unprocessed projectedAccount
          streamPositions = Message.messageStreamPosition . Projection.message <$> unprocessed
          reasons = fmap Projection.reason unprocessed
          state = Projection.state projectedAccount

      streamPositions `shouldBe` [0 .. 4]
      reasons `shouldBe` replicate 5 Handlers.HandlerNotFound
      Projection.version projectedAccount `shouldBe` Functions.DoesNotExist
      Projection.versionIncludingUnprocessed projectedAccount `shouldBe` Functions.DoesExist 4
      state `shouldBe` Projection.initial BankAccount.projection

    it "conversion errors are reported correctly" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      TestApp.runWith testAppData $ do
        BankAccount.send accountId (BankAccount.Open 200)
        BankAccount.send accountId (BankAccount.Open 202)
        BankAccount.send accountId (BankAccount.Deposit 20)
        BankAccount.send accountId (BankAccount.Deposit 14)
        BankAccount.send accountId (BankAccount.Withdraw 100)
        TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 5

      Just projectedAccount <-
        TestApp.runWith testAppData $ fetchWithConversionErrors accountId

      let unprocessed = Projection.unprocessed projectedAccount
          streamPositions = Message.messageStreamPosition . Projection.message <$> unprocessed
          reasons = fmap Projection.reason unprocessed
          state = Projection.state projectedAccount
          expectedReason =
            Handlers.HandlerParseFailure $
              Message.ParseMessageFailure
                { failedPayloadReason = Just "Error in $: expected Bool, but encountered Object"
                , failedMetadataReason = Just "Error in $: expected Bool, but encountered Object"
                }

      streamPositions `shouldBe` [0 .. 4]
      Projection.version projectedAccount `shouldBe` Functions.DoesNotExist
      Projection.versionIncludingUnprocessed projectedAccount `shouldBe` Functions.DoesExist 4
      state `shouldBe` Projection.initial BankAccount.projection
      reasons `shouldBe` replicate 5 expectedReason
