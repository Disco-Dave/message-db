module MessageDb.ProjectionSpec (spec) where

import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask))
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.Functor ((<&>))
import qualified Data.Pool as Pool
import Data.Semigroup (Endo (Endo))
import Data.Set (Set)
import qualified Data.Set as Set
import Examples.BankAccount (BankAccount)
import qualified Examples.BankAccount as BankAccount
import GHC.Generics (Generic)
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (MessageId)
import qualified MessageDb.Message as Message
import MessageDb.Projection (Projected, Projection (handlers, initial))
import qualified MessageDb.Projection as Projection
import MessageDb.StreamName (StreamName (..))
import Test.Hspec (Spec, around, describe, it, shouldBe)
import TestApp (TestApp, TestAppData (..))
import qualified TestApp


data BankAccountSnapshot = BankAccountSnapshot
  { bankAccount :: BankAccount
  , messagesProcessed :: Set MessageId
  }
  deriving (Show, Eq, Generic)


instance Aeson.ToJSON BankAccountSnapshot


instance Aeson.FromJSON BankAccountSnapshot where
  parseJSON = Aeson.withObject "BankAccountSnapshot" $ \object ->
    BankAccountSnapshot
      <$> object .: "bankAccount"
      -- Always set messagesProcessed to empty set when reding from json.
      -- This way we can tell what messages were actually processed when restored from a snapshot.
      <*> pure Set.empty


snapshotStream :: BankAccount.AccountId -> Projection.SnapshotStreamName
snapshotStream accountId =
  "snapshots-" <> coerce accountId


fetchWithSnapshots :: BankAccount.AccountId -> TestApp (Maybe (Projected BankAccountSnapshot))
fetchWithSnapshots accountId = do
  TestAppData{connectionPool} <- ask

  let modifiedHandlers =
        Projection.handlers BankAccount.projection
          <&> \oldHandler -> do
            message <- Handlers.getMessage

            (Endo updateBankAccount) <- oldHandler

            pure . Endo $ \state ->
              state
                { bankAccount = updateBankAccount $ bankAccount state
                , messagesProcessed = Set.insert (Message.messageId message) (messagesProcessed state)
                }

      modifiedProjection =
        BankAccount.projection
          { initial =
              BankAccountSnapshot
                { bankAccount = Projection.initial BankAccount.projection
                , messagesProcessed = Set.empty
                }
          , handlers = modifiedHandlers
          }

  liftIO $
    Projection.fetchWithSnapshots
      (Pool.withResource connectionPool)
      (Functions.FixedSize 100)
      (BankAccount.entityStream accountId)
      modifiedProjection
      (snapshotStream accountId)


spec :: Spec
spec =
  around (TestApp.withSubscriptions [BankAccount.subscribe]) . describe "fetchWithSnapshots" $ do
    it "writes a snapshot when done" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      void . TestApp.runWith testAppData $ do
        BankAccount.send accountId (BankAccount.Open 200)
        BankAccount.send accountId (BankAccount.Open 202)
        BankAccount.send accountId (BankAccount.Deposit 20)
        BankAccount.send accountId (BankAccount.Deposit 14)
        BankAccount.send accountId (BankAccount.Withdraw 100)
        TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 5
        fetchWithSnapshots accountId

      snapshots <-
        TestApp.runWith testAppData . TestApp.withConnection $ \connection ->
          liftIO $
            Functions.getStreamMessages
              connection
              (coerce $ snapshotStream accountId)
              Nothing
              Nothing
              Nothing

      length snapshots `shouldBe` 1

    it "doesn't write the same snapshot twice" $ \testAppData -> do
      accountId <- BankAccount.newAccountId

      Just firstProjection <-
        TestApp.runWith testAppData $ do
          BankAccount.send accountId (BankAccount.Open 200)
          BankAccount.send accountId (BankAccount.Open 202)
          BankAccount.send accountId (BankAccount.Deposit 20)
          BankAccount.send accountId (BankAccount.Deposit 14)
          BankAccount.send accountId (BankAccount.Withdraw 100)
          TestApp.blockUntilStreamHas (BankAccount.entityStream accountId) 5
          fetchWithSnapshots accountId

      Just secondProjection <-
        TestApp.runWith testAppData $
          fetchWithSnapshots accountId

      let countMessagesProcessed =
            Set.size . messagesProcessed . Projection.state

          hasCorrectState projection = do
            let actualState = bankAccount $ Projection.state projection

            Set.size (BankAccount.commandsProcessed actualState) `shouldBe` 5
            BankAccount.balance actualState `shouldBe` 134
            BankAccount.isOpened actualState `shouldBe` True
            BankAccount.overdrafts actualState `shouldBe` []

      countMessagesProcessed firstProjection `shouldBe` 5
      countMessagesProcessed secondProjection `shouldBe` 0

      hasCorrectState firstProjection
      hasCorrectState secondProjection

      Projection.version firstProjection `shouldBe` Functions.DoesExist 4
      Projection.version secondProjection `shouldBe` Functions.DoesExist 4

      Projection.unprocessed firstProjection `shouldBe` []
      Projection.unprocessed secondProjection `shouldBe` []

      snapshots <-
        TestApp.runWith testAppData . TestApp.withConnection $ \connection ->
          liftIO $
            Functions.getStreamMessages
              connection
              (coerce $ snapshotStream accountId)
              Nothing
              Nothing
              Nothing

      length snapshots `shouldBe` 1
