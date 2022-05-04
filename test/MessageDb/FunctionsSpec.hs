module MessageDb.FunctionsSpec
  ( spec,
  )
where

import Control.Monad (replicateM_)
import Data.Foldable (for_)
import Data.Text.Encoding (encodeUtf8)
import qualified Database.PostgreSQL.Simple as Simple
import Generators.Message (genGlobalPosition, genMessageType, genMetadata, genPayload)
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
    describe "lookupById" $ do
      it "returns nothing when no message exists with id" $ \connection -> do
        messageId <- Message.newMessageId
        message <- Functions.lookupById connection messageId
        message `shouldBe` Nothing

      it "returns correct message when message exists with id" $ \connection -> do
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

        message <- Functions.lookupById connection messageId

        fmap Message.messageId message `shouldBe` Just messageId
        fmap Message.streamName message `shouldBe` Just streamName
        fmap Message.messageType message `shouldBe` Just messageType
        (Message.payload =<< message) `shouldBe` Just payload
        (Message.metadata =<< message) `shouldBe` Just metadata

    describe "lookupByPosition" $ do
      it "returns nothing when no message exists at global position" $ \connection -> do
        globalPosition <- Gen.sample genGlobalPosition
        message <- Functions.lookupByPosition connection globalPosition
        message `shouldBe` Nothing

      it "returns correct message when message exists at global position" $ \connection -> do
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

        Just expectedMessage <- Functions.lookupById connection messageId
        Just actualMessage <- Functions.lookupByPosition connection (Message.globalPosition expectedMessage)

        actualMessage `shouldBe` expectedMessage

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

    describe "getStreamMessages" $ do
      it "returns an empty list when there are no messages" $ \connection -> do
        rows <-
          Functions.getStreamMessages
            connection
            "foobar-2000"
            Nothing
            Nothing
            Nothing

        rows `shouldBe` []

      it "returns messages when there are messages in the stream" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 10 $ do
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        messages <-
          Functions.getStreamMessages
            connection
            streamName
            Nothing
            Nothing
            Nothing

        length messages `shouldBe` 10

        for_ (zip [0 .. 9] messages) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

      it "returns messages after specified stream position" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 10 $ do
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        messages <-
          Functions.getStreamMessages
            connection
            streamName
            (Just 5)
            Nothing
            Nothing

        length messages `shouldBe` 5

        for_ (zip [5 .. 9] messages) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

      it "returns less than or equal to batch size when specified" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 18 $ do
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        firstBatch <-
          Functions.getStreamMessages
            connection
            streamName
            Nothing
            (Just $ Functions.FixedSize 5)
            Nothing

        secondBatch <-
          Functions.getStreamMessages
            connection
            streamName
            (Just 5)
            (Just $ Functions.FixedSize 5)
            Nothing

        thirdBatch <-
          Functions.getStreamMessages
            connection
            streamName
            (Just 10)
            (Just $ Functions.FixedSize 5)
            Nothing

        fourthBatch <-
          Functions.getStreamMessages
            connection
            streamName
            (Just 15)
            (Just $ Functions.FixedSize 5)
            Nothing

        length firstBatch `shouldBe` 5
        length secondBatch `shouldBe` 5
        length thirdBatch `shouldBe` 5
        length fourthBatch `shouldBe` 3

        for_ (zip [0 .. 4] firstBatch) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [5 .. 9] secondBatch) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [10 .. 14] thirdBatch) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [15 .. 17] fourthBatch) $ \(index, message) -> do
          Message.streamPosition message `shouldBe` Message.StreamPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

      it "returns everything when a batch size of unlimited is specified" $ const pending

      it "returns messages that match the condition when specified" $ \connection -> do
        streamName <- Gen.sample genStreamName
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        messageType1 <- Gen.sample genMessageType
        messageType2 <- Gen.sample genMessageType

        for_ [0 :: Int .. 9] $ \index -> do
          Functions.writeMessage
            connection
            streamName
            (if odd index then messageType1 else messageType2)
            payload
            metadata
            Nothing

        messages <-
          Functions.getStreamMessages
            connection
            streamName
            Nothing
            Nothing
            (Just . Functions.Condition $ "type <> '" <> Message.fromMessageType messageType2 <> "'")

        length messages `shouldBe` 5
        messages `shouldSatisfy` all ((== messageType1) . Message.messageType)
        messages `shouldSatisfy` all (odd . Message.fromStreamPosition . Message.streamPosition)

    describe "getCategoryMessages" $ do
      it "returns nothing when no messages exist in category" $ const pending
      it "returns messages when there are messages in the category" $ const pending
      it "returns messages after specified position" $ const pending
      it "returns less than or equal to batch size of messages" $ const pending
      it "returns everything when a batch size of unlimited is specified" $ const pending
      it "returns correct messages when consumer group is used" $ const pending
      it "returns messages that match the condition when specified" $ const pending

    describe "getLastStreamMessage" $ do
      it "returns nothing when there are no messages in stream" $ const pending
      it "returns the last message when messages exist in stream" $ const pending

    describe "streamVersion" $ do
      it "returns nothing when stream is empty" $ const pending
      it "returns the last position when stream is not empty" $ const pending
