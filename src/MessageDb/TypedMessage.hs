module MessageDb.TypedMessage (
  TypedMessage (..),
  ConversionFailure (..),
  typed,
) where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message

data TypedMessage payload metadata = TypedMessage
  { messageId :: Message.MessageId
  , streamName :: Message.StreamName
  , messageType :: Message.MessageType
  , streamPosition :: Message.StreamPosition
  , globalPosition :: Message.GlobalPosition
  , payload :: payload
  , metadata :: metadata
  , createdAtTimestamp :: Message.CreatedAtTimestamp
  }
  deriving (Show, Eq)

data ConversionFailure = ConversionFailure
  { failedPayloadReason :: Maybe String
  , failedMetadataReason :: Maybe String
  }
  deriving (Show, Eq)
instance Exception ConversionFailure

typed :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => Message -> Either ConversionFailure (TypedMessage payload metadata)
typed message =
  case (Message.typedPayload message, Message.typedMetadata message) of
    (Right typedPayload, Right typedMetadata) ->
      Right $
        TypedMessage
          { messageId = Message.messageId message
          , streamName = Message.streamName message
          , messageType = Message.messageType message
          , streamPosition = Message.streamPosition message
          , globalPosition = Message.globalPosition message
          , payload = typedPayload
          , metadata = typedMetadata
          , createdAtTimestamp = Message.createdAtTimestamp message
          }
    (payloadResult, metadataResult) ->
      let toMaybe = either Just (const Nothing)
       in Left $
            ConversionFailure
              { failedPayloadReason = toMaybe payloadResult
              , failedMetadataReason = toMaybe metadataResult
              }
