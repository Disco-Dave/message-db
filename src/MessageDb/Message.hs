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
import MessageDb.StreamName (StreamName (..))

-- | Identifier of a message record
newtype MessageId = MessageId
  { fromMessageId :: UUID
  }
  deriving (Show, Eq, Ord)

-- | Make a new unique 'MessageId'
newMessageId :: IO MessageId
newMessageId =
  fmap MessageId UUID.V4.nextRandom

-- | The type of the message
newtype MessageType = MessageType
  { fromMessageType :: Text
  }
  deriving (Show, Eq)

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
