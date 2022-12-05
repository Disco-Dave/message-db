module MessageDb.Message.MessageType
  ( MessageType (..)
  , messageTypeFromText
  , HasMessageType (..)
  , MessageTypeIs (..)
  )
where

import qualified Data.Aeson as Aeson
import Data.Proxy (Proxy (Proxy))
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)


-- | The type of a message. You can use this later to determine what kind of event or command a message is.
newtype MessageType = MessageType
  { messageTypeToText :: Text
  -- ^ Convert a 'MessageType' to a 'Text'.
  }
  deriving
    ( Eq
    , Ord
    , Semigroup
    , Monoid
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


class HasMessageType payload where
  getMessageType :: MessageType


newtype MessageTypeIs (messageType :: Symbol) payload = MessageTypeIs payload


instance KnownSymbol messageType => HasMessageType (MessageTypeIs messageType payload) where
  getMessageType =
    MessageType . Text.pack $ symbolVal @messageType Proxy
