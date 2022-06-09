module Generators.Message
  ( genMessageId,
    genMessageType,
    genStreamPosition,
    genGlobalPosition,
    genPayload,
    genMetadata,
    genCreatedAtTimestamp,
    genMessage,
  )
where

import Generators (genAesonValue, genUTCTime, genUUID)
import Generators.StreamName (genStreamName)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MessageDb.Message (Message (Message))
import qualified MessageDb.Message as Message


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


genCreatedAtTimestamp :: Gen Message.CreatedAt
genCreatedAtTimestamp =
  fmap Message.CreatedAt genUTCTime


genMessage :: Gen Message
genMessage = do
  messageId <- genMessageId
  messageStream <- genStreamName
  messageType <- genMessageType
  messageStreamPosition <- genStreamPosition
  messageGlobalPosition <- genGlobalPosition
  messagePayload <- genPayload
  messageMetadata <- genMetadata
  messageCreatedAt <- genCreatedAtTimestamp

  pure Message{..}
