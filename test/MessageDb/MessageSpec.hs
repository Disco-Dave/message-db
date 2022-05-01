module MessageDb.MessageSpec
  ( genMessageId,
    genMessageType,
    genStreamPosition,
    genGlobalPosition,
    genPayload,
    genMetadata,
    genCreatedAtTimestamp,
    genMessage,
    spec,
  )
where

import Generators (genAesonValue, genUTCTime, genUUID)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MessageDb.Message (Message (Message))
import qualified MessageDb.Message as Message
import MessageDb.StreamNameSpec (genStreamName)
import Test.Hspec


genMessageId :: Gen Message.MessageId
genMessageId =
  fmap Message.MessageId genUUID


genMessageType :: Gen Message.MessageType
genMessageType =
  fmap Message.MessageType $
    let range = Range.linear 20 100
     in Gen.text range Gen.alphaNum


genStreamPosition :: Gen Message.StreamPosition
genStreamPosition =
  Message.StreamPosition <$> Gen.integral (Range.constant 0 1_000_000)


genGlobalPosition :: Gen Message.GlobalPosition
genGlobalPosition =
  Message.GlobalPosition <$> Gen.integral (Range.constant 0 1_000_000)


genPayload :: Gen Message.Payload
genPayload =
  fmap Message.Payload genAesonValue


genMetadata :: Gen Message.Metadata
genMetadata =
  fmap Message.Metadata genAesonValue


genCreatedAtTimestamp :: Gen Message.CreatedAtTimestamp
genCreatedAtTimestamp =
  fmap Message.CreatedAtTimestamp genUTCTime


genMessage :: Gen Message
genMessage = do
  messageId <- genMessageId
  streamName <- genStreamName
  messageType <- genMessageType
  streamPosition <- genStreamPosition
  globalPosition <- genGlobalPosition
  payload <- Gen.maybe genPayload
  metadata <- Gen.maybe genMetadata
  createdAtTimestamp <- genCreatedAtTimestamp

  pure Message{..}


spec :: Spec
spec =
  pure ()
