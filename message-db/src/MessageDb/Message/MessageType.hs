module MessageDb.Message.MessageType
  ( MessageType (..)
  , messageTypeFromText
  )
where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)


-- | The type of a message. You can use this later to determine what kind of event or command a message is.
newtype MessageType = MessageType
  { messageTypeToText :: Text
  -- ^ Convert a 'MessageType' to a 'Text'.
  }
  deriving
    ( Eq
    , Ord
    , IsString
    , Aeson.ToJSON
    , Aeson.FromJSON
    , FromField
    , ToField
    )
  deriving (Show) via Text


-- | Convert a 'Text to a 'MessageType'.
messageTypeFromText :: Text -> MessageType
messageTypeFromText =
  MessageType
