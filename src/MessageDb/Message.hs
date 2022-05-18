module MessageDb.Message
  ( MessageId (..),
    newMessageId,
    MessageType (..),
    StreamPosition (..),
    GlobalPosition (..),
    CreatedAtTimestamp (..),
    Payload (..),
    Metadata (..),
    Message (..),
    parsePayload,
    nullPayload,
    typeOf,
    typedPayload,
    parseMetadata,
    typedMetadata,
    nullMetadata,
  )
where

import Data.Aeson ((.!=), (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Coerce (coerce)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (UTCTime)
import Data.Typeable (Typeable, typeRep)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import MessageDb.StreamName (StreamName (..))


newtype MessageId = MessageId
  { fromMessageId :: UUID
  }
  deriving (Eq, Ord)
  deriving (Show) via UUID


newMessageId :: IO MessageId
newMessageId =
  fmap MessageId UUID.V4.nextRandom


instance Aeson.ToJSON MessageId where
  toJSON = Aeson.toJSON . fromMessageId
  toEncoding = Aeson.toEncoding . fromMessageId


instance Aeson.FromJSON MessageId where
  parseJSON = fmap MessageId . Aeson.parseJSON


newtype MessageType = MessageType
  { fromMessageType :: Text
  }
  deriving (Eq, Ord, IsString)
  deriving (Show) via Text


typeOf :: forall payload. Typeable payload => MessageType
typeOf =
  let eventName = Text.pack . show . typeRep $ Proxy @payload
   in MessageType eventName


instance Aeson.ToJSON MessageType where
  toJSON = Aeson.toJSON . fromMessageType
  toEncoding = Aeson.toEncoding . fromMessageType


instance Aeson.FromJSON MessageType where
  parseJSON = fmap MessageType . Aeson.parseJSON


newtype StreamPosition = StreamPosition
  { fromStreamPosition :: Integer
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Integer


instance Aeson.ToJSON StreamPosition where
  toJSON = Aeson.toJSON . fromStreamPosition
  toEncoding = Aeson.toEncoding . fromStreamPosition


instance Aeson.FromJSON StreamPosition where
  parseJSON = fmap StreamPosition . Aeson.parseJSON


-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { fromGlobalPosition :: Integer
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Integer


instance Aeson.ToJSON GlobalPosition where
  toJSON = Aeson.toJSON . fromGlobalPosition
  toEncoding = Aeson.toEncoding . fromGlobalPosition


instance Aeson.FromJSON GlobalPosition where
  parseJSON = fmap GlobalPosition . Aeson.parseJSON


-- | Message payload
newtype Payload = Payload
  { fromPayload :: Aeson.Value
  }
  deriving (Eq)
  deriving (Show) via Aeson.Value


nullPayload :: Payload
nullPayload =
  Payload Aeson.Null


instance Aeson.ToJSON Payload where
  toJSON = Aeson.toJSON . fromPayload
  toEncoding = Aeson.toEncoding . fromPayload


instance Aeson.FromJSON Payload where
  parseJSON = fmap Payload . Aeson.parseJSON


-- | Message metadata
newtype Metadata = Metadata
  { fromMetadata :: Aeson.Value
  }
  deriving (Eq)
  deriving (Show) via Aeson.Value


nullMetadata :: Metadata
nullMetadata =
  Metadata Aeson.Null


instance Aeson.ToJSON Metadata where
  toJSON = Aeson.toJSON . fromMetadata
  toEncoding = Aeson.toEncoding . fromMetadata


instance Aeson.FromJSON Metadata where
  parseJSON = fmap Metadata . Aeson.parseJSON


-- | Timestamp when the message was written.
newtype CreatedAtTimestamp = CreatedAtTimestamp
  { fromCreatedAtTimestamp :: UTCTime
  }
  deriving (Eq, Ord)
  deriving (Show) via UTCTime


instance Aeson.ToJSON CreatedAtTimestamp where
  toJSON = Aeson.toJSON . fromCreatedAtTimestamp
  toEncoding = Aeson.toEncoding . fromCreatedAtTimestamp


instance Aeson.FromJSON CreatedAtTimestamp where
  parseJSON = fmap CreatedAtTimestamp . Aeson.parseJSON


data Message = Message
  { messageId :: MessageId
  , streamName :: StreamName
  , messageType :: MessageType
  , streamPosition :: StreamPosition
  , globalPosition :: GlobalPosition
  , payload :: Payload
  , metadata :: Metadata
  , createdAtTimestamp :: CreatedAtTimestamp
  }
  deriving (Show, Eq)


parseJson :: Aeson.FromJSON value => Aeson.Value -> Either String value
parseJson column =
  let json = coerce column
   in AesonTypes.parseEither Aeson.parseJSON json


parsePayload :: Aeson.FromJSON value => Payload -> Either String value
parsePayload =
  parseJson . coerce


typedPayload :: Aeson.FromJSON payload => Message -> Either String payload
typedPayload Message{payload} =
  parsePayload payload


parseMetadata :: Aeson.FromJSON value => Metadata -> Either String value
parseMetadata =
  parseJson . coerce


typedMetadata :: Aeson.FromJSON metadata => Message -> Either String metadata
typedMetadata Message{metadata} =
  parseJson $ coerce metadata


toKeyValues :: Aeson.KeyValue keyValue => Message -> [keyValue]
toKeyValues Message{..} =
  [ "id" .= messageId
  , "streamName" .= streamName
  , "type" .= messageType
  , "streamPosition" .= streamPosition
  , "globalPosition" .= globalPosition
  , "payload" .= payload
  , "metadata" .= metadata
  , "createdAtTimestamp" .= createdAtTimestamp
  ]


instance Aeson.ToJSON Message where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance Aeson.FromJSON Message where
  parseJSON = Aeson.withObject "Message" $ \object -> do
    messageId <- object .: "id"
    streamName <- object .: "streamName"
    messageType <- object .: "type"
    streamPosition <- object .: "streamPosition"
    globalPosition <- object .: "globalPosition"
    payload <- object .:? "payload" .!= nullPayload
    metadata <- object .:? "metadata" .!= nullMetadata
    createdAtTimestamp <- object .: "createdAtTimestamp"
    pure Message{..}
