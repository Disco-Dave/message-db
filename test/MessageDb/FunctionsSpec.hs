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
import qualified Hedgehog.Gen as Gen
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName (..))
import TempMessageDb (withConnection)
import Test.Hspec
import UnliftIO.Exception (try)


spec :: Spec
spec =
  around withConnection $ do
    describe "writeMessage" $ do
      it "can write a message with metadata" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample genMetadata

        (messageId, _) <-
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            (Just metadata)
            Nothing

        Just message <-
          Functions.lookupById connection messageId

        Message.messageId message `shouldBe` messageId
        Message.streamName message `shouldBe` streamName
        Message.messageType message `shouldBe` messageType
        Message.payload message `shouldBe` Just payload
        Message.metadata message `shouldBe` Just metadata

      it "can write a message without metadata" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload

        (messageId, _) <-
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            (Nothing :: Maybe ())
            Nothing

        Just message <-
          Functions.lookupById connection messageId

        Message.messageId message `shouldBe` messageId
        Message.streamName message `shouldBe` streamName
        Message.messageType message `shouldBe` messageType
        Message.payload message `shouldBe` Just payload
        Message.metadata message `shouldBe` Nothing

      it "throws exception when expected version check fails" $ \connection -> do
        streamName <- Gen.sample genStreamName

        replicateM_ 10 $ do
          messageType <- Gen.sample genMessageType

          payload <- Gen.sample genPayload
          metadata <- Gen.sample $ Gen.maybe genMetadata

          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        _ <- do
          messageType <- Gen.sample genMessageType

          payload <- Gen.sample genPayload
          metadata <- Gen.sample $ Gen.maybe genMetadata

          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            (Just 9)

        results <- do
          messageType <- Gen.sample genMessageType

          payload <- Gen.sample genPayload
          metadata <- Gen.sample $ Gen.maybe genMetadata

          try @_ @Simple.SqlError $
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

        actualErrorMessage `shouldBe` expectedErrorMessage
