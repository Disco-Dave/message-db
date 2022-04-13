module MessageDb.Message (
  MessageId (..),
  newMessageId,
  MessageType (..),
  StreamPosition (..),
  GlobalPosition (..),
  CreatedAtTimestamp (..),
  Payload (..),
  Metadata (..),
  StreamName (..),
  Message (..),
) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Simple.ToField as ToField
import MessageDb.StreamName (StreamName (..))

-- * Message Ids musut be unique per message across the entire event store.

-- | Identifier of a message record
newtype MessageId = MessageId
  { fromMessageId :: UUID
  }
  deriving (Show, Eq, Ord)

-- | Make a new unique 'MessageId'
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

-- | The type of the message
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

-- | The ordinal position of the message in its stream. Position is gapless.
newtype StreamPosition = StreamPosition
  { fromStreamPosition :: Integer
  }
  deriving (Show, Eq, Ord)

-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { fromGlobalPosition :: Integer
  }
  deriving (Show, Eq, Ord)

-- | Message payload
newtype Payload = Payload
  { fromPayload :: Aeson.Value
  }
  deriving (Show, Eq)

-- | Message metadata
newtype Metadata = Metadata
  { fromMetadata :: Aeson.Value
  }
  deriving (Show, Eq)

-- | Timestamp when the message was written.
newtype CreatedAtTimestamp = CreatedAtTimestamp
  { fromCreatedAtTimestamp :: UTCTime
  }
  deriving (Show, Eq, Ord)

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
