{-# LANGUAGE ScopedTypeVariables #-}

module MessageDb.Message (
  MessageId (..),
  newMessageId,
  MessageType (..),
  StreamPosition (..),
  GlobalPosition (..),
  CreatedAtTimestamp (..),
  Payload (..),
  Metadata (..),
  typedPayload,
  typedMetadata,
  Message (..),
  StreamName (..),
  StreamName.all,
  StreamName.CategoryName,
  StreamName.fromCategoryName,
  StreamName.category,
  StreamName.IdentityName,
  StreamName.identity,
  StreamName.fromIdentityName,
) where

import Data.Aeson ((.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Coerce (coerce)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Simple.ToField as ToField
import MessageDb.StreamName (StreamName (..))
import qualified MessageDb.StreamName as StreamName

newtype MessageId = MessageId
  { fromMessageId :: UUID
  }
  deriving (Show, Eq, Ord)

newMessageId :: IO MessageId
newMessageId =
  fmap MessageId UUID.V4.nextRandom

instance Aeson.ToJSON MessageId where
  toJSON = Aeson.toJSON . fromMessageId
  toEncoding = Aeson.toEncoding . fromMessageId

instance Aeson.FromJSON MessageId where
  parseJSON = fmap MessageId . Aeson.parseJSON

instance ToField MessageId where
  toField = ToField.toField . fromMessageId

instance FromField MessageId where
  fromField = fmap (fmap MessageId) . FromField.fromField

newtype MessageType = MessageType
  { fromMessageType :: Text
  }
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON MessageType where
  toJSON = Aeson.toJSON . fromMessageType
  toEncoding = Aeson.toEncoding . fromMessageType

instance Aeson.FromJSON MessageType where
  parseJSON = fmap MessageType . Aeson.parseJSON

instance ToField MessageType where
  toField = ToField.toField . fromMessageType

instance FromField MessageType where
  fromField = fmap (fmap MessageType) . FromField.fromField

newtype StreamPosition = StreamPosition
  { fromStreamPosition :: Integer
  }
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON StreamPosition where
  toJSON = Aeson.toJSON . fromStreamPosition
  toEncoding = Aeson.toEncoding . fromStreamPosition

instance Aeson.FromJSON StreamPosition where
  parseJSON = fmap StreamPosition . Aeson.parseJSON

instance ToField StreamPosition where
  toField = ToField.toField . fromStreamPosition

instance FromField StreamPosition where
  fromField = fmap (fmap StreamPosition) . FromField.fromField

-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { fromGlobalPosition :: Integer
  }
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON GlobalPosition where
  toJSON = Aeson.toJSON . fromGlobalPosition
  toEncoding = Aeson.toEncoding . fromGlobalPosition

instance Aeson.FromJSON GlobalPosition where
  parseJSON = fmap GlobalPosition . Aeson.parseJSON

instance ToField GlobalPosition where
  toField = ToField.toField . fromGlobalPosition

instance FromField GlobalPosition where
  fromField = fmap (fmap GlobalPosition) . FromField.fromField

-- | Message payload
newtype Payload = Payload
  { fromPayload :: Aeson.Value
  }
  deriving (Show, Eq)

instance Aeson.ToJSON Payload where
  toJSON = Aeson.toJSON . fromPayload
  toEncoding = Aeson.toEncoding . fromPayload

instance Aeson.FromJSON Payload where
  parseJSON = fmap Payload . Aeson.parseJSON

instance ToField Payload where
  toField = ToField.toField . fromPayload

instance FromField Payload where
  fromField = fmap (fmap Payload) . FromField.fromField

-- | Message metadata
newtype Metadata = Metadata
  { fromMetadata :: Aeson.Value
  }
  deriving (Show, Eq)

instance Aeson.ToJSON Metadata where
  toJSON = Aeson.toJSON . fromMetadata
  toEncoding = Aeson.toEncoding . fromMetadata

instance Aeson.FromJSON Metadata where
  parseJSON = fmap Metadata . Aeson.parseJSON

instance ToField Metadata where
  toField = ToField.toField . fromMetadata

instance FromField Metadata where
  fromField = fmap (fmap Metadata) . FromField.fromField

-- | Timestamp when the message was written.
newtype CreatedAtTimestamp = CreatedAtTimestamp
  { fromCreatedAtTimestamp :: UTCTime
  }
  deriving (Show, Eq, Ord)

instance Aeson.ToJSON CreatedAtTimestamp where
  toJSON = Aeson.toJSON . fromCreatedAtTimestamp
  toEncoding = Aeson.toEncoding . fromCreatedAtTimestamp

instance Aeson.FromJSON CreatedAtTimestamp where
  parseJSON = fmap CreatedAtTimestamp . Aeson.parseJSON

instance ToField CreatedAtTimestamp where
  toField = ToField.toField . fromCreatedAtTimestamp

instance FromField CreatedAtTimestamp where
  fromField = fmap (fmap CreatedAtTimestamp) . FromField.fromField

data Message = Message
  { messageId :: MessageId
  , streamName :: StreamName
  , messageType :: MessageType
  , streamPosition :: StreamPosition
  , globalPosition :: GlobalPosition
  , payload :: Maybe Payload
  , metadata :: Maybe Metadata
  , createdAtTimestamp :: CreatedAtTimestamp
  }
  deriving (Show, Eq)

typedField :: Aeson.FromJSON value => Maybe Aeson.Value -> Either String value
typedField column =
  let json = fromMaybe (Aeson.toJSON @(Maybe ()) Nothing) (coerce column)
   in AesonTypes.parseEither Aeson.parseJSON json

typedPayload :: Aeson.FromJSON payload => Message -> Either String payload
typedPayload Message{payload} =
  typedField $ coerce payload

typedMetadata :: Aeson.FromJSON metadata => Message -> Either String metadata
typedMetadata Message{metadata} =
  typedField $ coerce metadata

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
    payload <- object .:? "payload"
    metadata <- object .:? "metadata"
    createdAtTimestamp <- object .: "createdAtTimestamp"
    pure Message{..}
