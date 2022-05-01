module MessageDb.FunctionsSpec
  ( spec,
  )
where

import Control.Monad (replicateM_)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as Simple
import Generators.Message (genMessageType, genMetadata, genPayload)
import Generators.StreamName (genStreamName)
import Hedgehog (forAll)
import qualified Hedgehog.Gen as Gen
import qualified MessageDb.Functions as Functions
import MessageDb.StreamName (StreamName (..))
import TempMessageDb (withConnection)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog, (===))
import UnliftIO.Exception (try)


spec :: Spec
spec =
  around withConnection $ do
    describe "writeMessage" $ do
      it "throws exception when expected version check fails" $ \connection -> hedgehog $ do
        streamName <- forAll genStreamName

        replicateM_ 10 $ do
          messageType <- forAll genMessageType

          payload <- forAll genPayload
          metadata <- forAll $ Gen.maybe genMetadata

          liftIO $
            Functions.writeMessage
              connection
              streamName
              messageType
              payload
              metadata
              Nothing

        _ <- do
          messageType <- forAll genMessageType

          payload <- forAll genPayload
          metadata <- forAll $ Gen.maybe genMetadata

          liftIO $
            Functions.writeMessage
              connection
              streamName
              messageType
              payload
              metadata
              (Just 9)

        results <- do
          messageType <- forAll genMessageType

          payload <- forAll genPayload
          metadata <- forAll $ Gen.maybe genMetadata

          liftIO . try @_ @Simple.SqlError $
            Functions.writeMessage
              connection
              streamName
              messageType
              payload
              metadata
              (Just 4)

        let actualErrorMessage =
              case results of
                Right _ -> ""
                Left err -> Simple.sqlErrorMsg err
            
            expectedErrorMessage =
              let streamNameBS = encodeUtf8 (fromStreamName streamName)
               in "Wrong expected version: 4 (Stream: " <> streamNameBS <> ", Stream Version: 10)"

        actualErrorMessage === expectedErrorMessage
