module MessageDb.Message
  ( MessageId (..),
    newMessageId,
    MessageType (..),
    messageTypeOf,
    StreamPosition (..),
    GlobalPosition (..),
    CreatedAt (..),
    Payload (..),
    nullPayload,
    parsePayload,
    Metadata (..),
    nullMetadata,
    parseMetadata,
    Message (..),
    ParseMessageFailure (..),
    ParsedMessage (..),
    parseMessage,
  )
where

import Control.Exception (Exception)
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import MessageDb.StreamName (StreamName)
import Numeric.Natural (Natural)


-- * JSON Helpers


parseJson :: Aeson.FromJSON value => Aeson.Value -> Either String value
parseJson column =
  let json = coerce column
   in AesonTypes.parseEither Aeson.parseJSON json


showValue :: Aeson.Value -> String
showValue =
  Char8.unpack . Aeson.encode


-- * Message Id


-- | Unique id of a message. Most be unique across the entire event store.
newtype MessageId = MessageId
  { messageIdToUUID :: UUID
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.FromJSON
    , Aeson.ToJSON
    )
    via UUID


-- | Create a new unique message id.
newMessageId :: IO MessageId
newMessageId =
  fmap MessageId UUID.V4.nextRandom


-- * Message Type


-- | The type of a message. You can use this later to determine what kind of event or command a message is.
newtype MessageType = MessageType
  { messageTypeToText :: Text
  }
  deriving
    ( Eq
    , Ord
    , Show
    , IsString
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Text


-- | Converts a type's name to a 'MessageType'. For example 'typeOf @Bool' will be 'MessageType "Bool"'.
messageTypeOf :: forall payload. Typeable payload => MessageType
messageTypeOf =
  let eventName = Text.pack . show . typeRep $ Proxy @payload
   in MessageType eventName


-- * Stream Position


-- | Position within a stream. This starts at 0 and has no gaps.
newtype StreamPosition = StreamPosition
  { streamPositionToNatural :: Natural
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Num
    , Real
    , Enum
    , Integral
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Natural


-- * Global Position


-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { globalPositionToInteger :: Integer
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Num
    , Real
    , Enum
    , Integral
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Integer


-- * Created at Timestamp


-- | Timestamp when the message was written.
newtype CreatedAt = CreatedAt
  { createdAtToUTCTime :: UTCTime
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via UTCTime


-- * Payload


newtype Payload = Payload
  { payloadToValue :: Aeson.Value
  }
  deriving
    ( Eq
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Aeson.Value


instance Show Payload where
  show = showValue . payloadToValue


nullPayload :: Payload
nullPayload =
  Payload Aeson.Null


parsePayload :: Aeson.FromJSON value => Payload -> Either String value
parsePayload =
  parseJson . payloadToValue


-- * Metadata


newtype Metadata = Metadata
  { metadataToValue :: Aeson.Value
  }
  deriving
    ( Eq
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
    via Aeson.Value


instance Show Metadata where
  show = showValue . metadataToValue


nullMetadata :: Metadata
nullMetadata =
  Metadata Aeson.Null


parseMetadata :: Aeson.FromJSON value => Metadata -> Either String value
parseMetadata =
  parseJson . metadataToValue


-- * Message


data Message = Message
  { messageId :: MessageId
  , messageStream :: StreamName
  , messageType :: MessageType
  , messageStreamPosition :: StreamPosition
  , messageGlobalPosition :: GlobalPosition
  , messageCreatedAt :: CreatedAt
  , messagePayload :: Payload
  , messageMetadata :: Metadata
  }
  deriving (Show, Eq)


toKeyValues :: Aeson.KeyValue keyValue => Message -> [keyValue]
toKeyValues Message{..} =
  [ "id" .= messageId
  , "streamName" .= messageStream
  , "type" .= messageType
  , "streamPosition" .= messageStreamPosition
  , "globalPosition" .= messageGlobalPosition
  , "createdAt" .= messageCreatedAt
  , "payload" .= messagePayload
  , "metadata" .= messageMetadata
  ]


instance Aeson.ToJSON Message where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \object -> do
    messageId <- object .: "id"
    messageStream <- object .: "streamName"
    messageType <- object .: "type"
    messageStreamPosition <- object .: "streamPosition"
    messageGlobalPosition <- object .: "globalPosition"
    messageCreatedAt <- object .: "createdAt"
    messagePayload <- object .:? "payload" .!= nullPayload
    messageMetadata <- object .:? "metadata" .!= nullMetadata
    pure
      Message
        { messageId = messageId
        , messageStream = messageStream
        , messageType = messageType
        , messageStreamPosition = messageStreamPosition
        , messageGlobalPosition = messageGlobalPosition
        , messageCreatedAt = messageCreatedAt
        , messagePayload = messagePayload
        , messageMetadata = messageMetadata
        }


-- * Parsed Message


data ParseMessageFailure = ParseMessageFailure
  { failedPayloadReason :: Maybe String
  , failedMetadataReason :: Maybe String
  }
  deriving (Show, Eq)


parseMessageFailureToKeyValues :: Aeson.KeyValue kv => ParseMessageFailure -> [kv]
parseMessageFailureToKeyValues ParseMessageFailure{..} =
  [ "failedPayloadReason" .= failedPayloadReason
  , "failedMetadataReason" .= failedMetadataReason
  ]


instance Aeson.ToJSON ParseMessageFailure where
  toJSON = Aeson.object . parseMessageFailureToKeyValues
  toEncoding = Aeson.pairs . mconcat . parseMessageFailureToKeyValues


instance Aeson.FromJSON ParseMessageFailure where
  parseJSON = Aeson.withObject "ParseMessageFailure" $ \object ->
    ParseMessageFailure
      <$> object .: "failedPayloadReason"
      <*> object .: "failedMetadataReason"


instance Exception ParseMessageFailure


data ParsedMessage payload metadata = ParsedMessage
  { parsedPayload :: payload
  , parsedMetadata :: metadata
  }
  deriving (Show, Eq)


parseMessage :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => Message -> Either ParseMessageFailure (ParsedMessage payload metadata)
parseMessage Message{messagePayload, messageMetadata} =
  case (parsePayload messagePayload, parseMetadata messageMetadata) of
    (Right parsedPayload, Right parsedMetadata) ->
      Right $ ParsedMessage parsedPayload parsedMetadata
    (payloadResult, metadataResult) ->
      let toMaybe = either Just (const Nothing)
       in Left $
            ParseMessageFailure
              { failedPayloadReason = toMaybe payloadResult
              , failedMetadataReason = toMaybe metadataResult
              }
