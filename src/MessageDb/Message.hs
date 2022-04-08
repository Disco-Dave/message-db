module MessageDb.Message (
  MessageId (..),
  newMessageId,
  StreamName (..),
  MessageType (..),
  StreamPosition (..),
  GlobalPosition (..),
  CreatedAtTimestamp (..),
  Message (..),
) where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUID.V4
import GHC.Generics (Generic)

-- | Identifier of a message record
newtype MessageId = MessageId
  { fromMessageId :: UUID
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )

-- | Make a new unique 'MessageId'
newMessageId :: IO MessageId
newMessageId =
  fmap MessageId UUID.V4.nextRandom

-- | Name of stream to which the message belongs
newtype StreamName = StreamName
  { fromStreamName :: Text
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , IsString
    )

-- | The type of the message
newtype MessageType = MessageType
  { fromMessageType :: Text
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , IsString
    )

-- | The ordinal position of the message in its stream. Position is gapless.
newtype StreamPosition = StreamPosition
  { fromStreamPosition :: Integer
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , Num
    )

-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { fromGlobalPosition :: Integer
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , Num
    )

-- | Message payload
newtype Payload = Payload
  { fromPayload :: Aeson.Value
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )

-- | Message metadata
newtype Metadata = Metadata
  { fromMetadata :: Aeson.Value
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )

-- | Timestamp when the message was written.
newtype CreatedAtTimestamp = CreatedAtTimestamp
  { fromCreatedAtTimestamp :: UTCTime
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    )

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
  deriving (Show, Eq, Ord, Generic)

instance Aeson.ToJSON Message
instance Aeson.FromJSON Message
