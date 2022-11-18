module MessageDb.Message
  ( Message (..)
  , UntypedMessage
  , parseMessagePayload
  , parseMessageMetadata
  , ParseMessageError (..)
  , parseMessage
  )
where

import Control.Exception (Exception)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import MessageDb.Message.CreatedAt (CreatedAt)
import MessageDb.Message.GlobalPosition (GlobalPosition)
import MessageDb.Message.MessageId (MessageId)
import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.Metadata (Metadata, fromMetadata)
import MessageDb.Message.Payload (Payload, fromPayload)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.Message.StreamPosition (StreamPosition)


data Message payload metadata = Message
  { messageId :: MessageId
  , messageStreamName :: StreamName
  , messageType :: MessageType
  , messageStreamPosition :: StreamPosition
  , messageGlobalPosition :: GlobalPosition
  , messageCreatedAt :: CreatedAt
  , messagePayload :: payload
  , messageMetadata :: metadata
  }
  deriving (Show, Eq)


toKeyValues
  :: ( Aeson.KeyValue keyValue
     , Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     )
  => Message payload metadata
  -> [keyValue]
toKeyValues Message{..} =
  [ "id" .= messageId
  , "streamName" .= messageStreamName
  , "type" .= messageType
  , "streamPosition" .= messageStreamPosition
  , "globalPosition" .= messageGlobalPosition
  , "createdAt" .= messageCreatedAt
  , "payload" .= messagePayload
  , "metadata" .= messageMetadata
  ]


instance (Aeson.ToJSON payload, Aeson.ToJSON metadata) => Aeson.ToJSON (Message payload metadata) where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance (Aeson.FromJSON payload, Aeson.FromJSON metadata) => Aeson.FromJSON (Message payload metadata) where
  parseJSON = Aeson.withObject "Message" $ \object -> do
    messageId <- object .: "id"
    messageStreamName <- object .: "streamName"
    messageType <- object .: "type"
    messageStreamPosition <- object .: "streamPosition"
    messageGlobalPosition <- object .: "globalPosition"
    messageCreatedAt <- object .: "createdAt"

    -- Make sure that a missing key is treated as null
    -- for both 'messagePayload' and 'messageMetadata'.

    jsonMessagePayload <- object .:? "payload" .!= Aeson.Null
    messagePayload <- Aeson.parseJSON jsonMessagePayload

    jsonMessageMetadata <- object .:? "metadata" .!= Aeson.Null
    messageMetadata <- Aeson.parseJSON jsonMessageMetadata

    pure Message{..}


parseMessagePayload
  :: Aeson.FromJSON payload
  => Message Payload metadata
  -> Either Text (Message payload metadata)
parseMessagePayload message = do
  parsedPayload <- fromPayload $ messagePayload message
  pure message{messagePayload = parsedPayload}


parseMessageMetadata
  :: Aeson.FromJSON metadata
  => Message payload Metadata
  -> Either Text (Message payload metadata)
parseMessageMetadata message = do
  parsedMetadata <- fromMetadata $ messageMetadata message
  pure message{messageMetadata = parsedMetadata}


type UntypedMessage = Message Payload Metadata


data ParseMessageError = ParseMessageError
  { payloadError :: Maybe Text
  , metadataError :: Maybe Text
  }
  deriving (Show, Eq, Generic)
instance Exception ParseMessageError
instance Aeson.ToJSON ParseMessageError
instance Aeson.FromJSON ParseMessageError


parseMessage
  :: (Aeson.FromJSON payload, Aeson.FromJSON metadata)
  => UntypedMessage
  -> Either ParseMessageError (Message payload metadata)
parseMessage message =
  let payloadResult = fromPayload $ messagePayload message
      metadataResult = fromMetadata $ messageMetadata message
   in case (payloadResult, metadataResult) of
        (Right parsedPayload, Right parsedMetadata) ->
          Right
            message
              { messagePayload = parsedPayload
              , messageMetadata = parsedMetadata
              }
        _ ->
          Left
            ParseMessageError
              { payloadError = either Just (const Nothing) payloadResult
              , metadataError = either Just (const Nothing) metadataResult
              }
