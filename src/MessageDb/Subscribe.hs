module MessageDb.Subscribe where

import Control.Exception (Exception, SomeException, throwIO)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import MessageDb.Functions
import MessageDb.Message
import MessageDb.StreamName
import Numeric.Natural (Natural)

newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString)

data SubscriptionSettings = SubscriptionSettings
  { subscriberId :: SubscriberId
  , categoryName :: CategoryName
  , messagesPerTick :: Natural
  , positionUpdateInterval :: Natural
  , tickIntervalMs :: Natural
  }

defaultSettings :: SubscriberId -> CategoryName -> SubscriptionSettings
defaultSettings subscriberId categoryName =
  SubscriptionSettings
    { subscriberId = subscriberId
    , categoryName = categoryName
    , messagesPerTick = 100
    , positionUpdateInterval = 100
    , tickIntervalMs = 100
    }

data Subscription = Subscription
  { settings :: SubscriptionSettings
  , handlers :: Map MessageType [Message -> IO ()]
  }

newtype ParseException = ParseException String
  deriving (Show)

instance Exception ParseException

parseOrThrow :: Either String json -> IO json
parseOrThrow =
  either (throwIO . ParseException) pure

registerAnything :: MessageType -> (Message -> IO ()) -> Subscription -> Subscription
registerAnything messageType handle subscription =
  let addHandler oldHandlers =
        Just $ handle : fromMaybe [] oldHandlers
   in subscription
        { handlers = Map.alter addHandler messageType (handlers subscription)
        }

register :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => MessageType -> (payload -> metadata -> IO ()) -> Subscription -> Subscription
register messageType handle handlers =
  let messageHandler message = do
        payload <- parseOrThrow $ typedPayload message
        metadata <- parseOrThrow $ typedMetadata message
        handle payload metadata
   in registerAnything messageType messageHandler handlers

start :: Subscription -> IO ()
start subscription = undefined
