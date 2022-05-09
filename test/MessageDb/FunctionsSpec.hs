module MessageDb.FunctionsSpec
  ( spec,
  )
where

import Control.Monad (replicateM_)
import Data.Foldable (for_)
import qualified Data.Set as Set
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Database.PostgreSQL.Simple as Simple
import Generators.Message (genGlobalPosition, genMessageType, genMetadata, genPayload)
import Generators.StreamName (genCategoryName, genIdentityName, genStreamName)
import qualified Hedgehog.Gen as Gen
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName (..))
import qualified MessageDb.StreamName as SteamName
import TempMessageDb (withConnection)
import Test.Hspec
import UnliftIO.Exception (try)


isCorrectTimestamp :: Time.UTCTime -> Time.NominalDiffTime -> Message.CreatedAtTimestamp -> Bool
isCorrectTimestamp now tolerance (Message.CreatedAtTimestamp timestamp) =
  abs (Time.diffUTCTime now timestamp) <= tolerance


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

        (messageId, position) <-
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            (Just metadata)
            Nothing

        now <- Time.getCurrentTime
        Just message <- Functions.lookupById connection messageId

        Message.messageId message `shouldBe` messageId
        Message.streamName message `shouldBe` streamName
        Message.messageType message `shouldBe` messageType
        Message.payload message `shouldBe` Just payload
        Message.metadata message `shouldBe` Just metadata
        Message.createdAtTimestamp message `shouldSatisfy` isCorrectTimestamp now 0.1
        position `shouldBe` 0

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
            (Just $ Functions.StreamVersion 9)

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
              (Just  $ Functions.StreamVersion 4)

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

      it "returns everything when a batch size of unlimited is specified" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        -- Default batch size is 1000 when not specified
        replicateM_ 1001 $ do
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
            (Just Functions.Unlimited)
            Nothing

        length messages `shouldBe` 1001

      it "returns up to 1000 messages when batch size is not specified" $ \connection -> do
        streamName <- Gen.sample genStreamName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 1001 $ do
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

        length messages `shouldBe` 1000

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
      it "returns empty list when no messages exist in category" $ \connection -> do
        categoryName <- Gen.sample genCategoryName

        messages <-
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            Nothing
            Nothing
            Nothing
            Nothing

        messages `shouldBe` []

      it "returns messages when there are messages in the category" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
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
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            Nothing
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

      it "returns messages after specified position" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
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
          Functions.getCategoryMessages
            connection
            categoryName
            (Just 5)
            Nothing
            Nothing
            Nothing
            Nothing

        length messages `shouldBe` 6

        for_ (zip [5 .. 9] messages) $ \(index, message) -> do
          Message.globalPosition message `shouldBe` Message.GlobalPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

      it "returns up to 1000 messages when batch size is not specified" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 1001 $ do
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

        length messages `shouldBe` 1000

      it "returns less than or equal to batch size of messages" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
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
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            (Just $ Functions.FixedSize 5)
            Nothing
            Nothing
            Nothing

        secondBatch <-
          Functions.getCategoryMessages
            connection
            categoryName
            (Just 6)
            (Just $ Functions.FixedSize 5)
            Nothing
            Nothing
            Nothing

        thirdBatch <-
          Functions.getCategoryMessages
            connection
            categoryName
            (Just 11)
            (Just $ Functions.FixedSize 5)
            Nothing
            Nothing
            Nothing

        fourthBatch <-
          Functions.getCategoryMessages
            connection
            categoryName
            (Just 16)
            (Just $ Functions.FixedSize 5)
            Nothing
            Nothing
            Nothing

        length firstBatch `shouldBe` 5
        length secondBatch `shouldBe` 5
        length thirdBatch `shouldBe` 5
        length fourthBatch `shouldBe` 3

        for_ (zip [1 .. 5] firstBatch) $ \(index, message) -> do
          Message.globalPosition message `shouldBe` Message.GlobalPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [6 .. 10] secondBatch) $ \(index, message) -> do
          Message.globalPosition message `shouldBe` Message.GlobalPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [11 .. 15] thirdBatch) $ \(index, message) -> do
          Message.globalPosition message `shouldBe` Message.GlobalPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

        for_ (zip [16 .. 18] fourthBatch) $ \(index, message) -> do
          Message.globalPosition message `shouldBe` Message.GlobalPosition index
          Message.streamName message `shouldBe` streamName
          Message.messageType message `shouldBe` messageType
          Message.payload message `shouldBe` Just payload
          Message.metadata message `shouldBe` metadata

      it "returns everything when a batch size of unlimited is specified" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        replicateM_ 1001 $ do
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        messages <-
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            (Just Functions.Unlimited)
            Nothing
            Nothing
            Nothing

        length messages `shouldBe` 1001

      it "returns correct messages when consumer group is used" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        messageType <- Gen.sample genMessageType
        payload <- Gen.sample genPayload
        metadata <- Gen.sample $ Gen.maybe genMetadata

        let group1 =
              Functions.ConsumerGroup
                { consumerGroupSize = 2
                , consumerGroupMember = 0
                }

            group2 =
              Functions.ConsumerGroup
                { consumerGroupSize = 2
                , consumerGroupMember = 1
                }

        replicateM_ 100 $ do
          identityName <- Gen.sample genIdentityName
          let streamName = SteamName.addIdentity categoryName identityName
          Functions.writeMessage
            connection
            streamName
            messageType
            payload
            metadata
            Nothing

        group1Messages <-
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            Nothing
            Nothing
            (Just group1)
            Nothing

        group2Messages <-
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            Nothing
            Nothing
            (Just group2)
            Nothing

        let group1Set = Set.fromList $ fmap Message.messageId group1Messages
            group2Set = Set.fromList $ fmap Message.messageId group2Messages

        length group1Messages + length group2Messages `shouldBe` 100

        group1Set `shouldNotSatisfy` Set.null
        group2Set `shouldNotSatisfy` Set.null

        group1Set `shouldNotBe` group2Set

      it "returns messages that match the condition when specified" $ \connection -> do
        categoryName <- Gen.sample genCategoryName
        identityName <- Gen.sample genIdentityName
        let streamName = SteamName.addIdentity categoryName identityName
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
          Functions.getCategoryMessages
            connection
            categoryName
            Nothing
            (Just Functions.Unlimited)
            Nothing
            Nothing
            (Just . Functions.Condition $ "type <> '" <> Message.fromMessageType messageType2 <> "'")

        length messages `shouldBe` 5
        messages `shouldSatisfy` all ((== messageType1) . Message.messageType)
        messages `shouldSatisfy` all (odd . Message.fromStreamPosition . Message.streamPosition)

    describe "getLastStreamMessage" $ do
      it "returns nothing when there are no messages in stream" $ \connection -> do
        streamName <- Gen.sample genStreamName
        message <- Functions.getLastStreamMessage connection streamName
        message `shouldBe` Nothing

      it "returns the last message when messages exist in stream" $ \connection -> do
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

        Just message <- Functions.getLastStreamMessage connection streamName

        Message.streamPosition message `shouldBe` 9

    describe "streamVersion" $ do
      it "returns nothing when stream is empty" $ \connection -> do
        streamName <- Gen.sample genStreamName
        position <- Functions.streamVersion connection streamName
        position `shouldBe` Nothing

      it "returns the last position when stream is not empty" $ \connection -> do
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

        Just position <- Functions.streamVersion connection streamName

        position `shouldBe` 9
