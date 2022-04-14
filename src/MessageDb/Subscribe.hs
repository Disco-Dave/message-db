module MessageDb.Subscribe (
  SubscriberId (..),
  Subscription (..),
  subscribe,
  ParseException (..),
  registerAnything,
  register,
  start,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import qualified Control.Immortal as Immortal
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import MessageDb.Functions ()
import MessageDb.Message (CategoryName, Message, MessageType, typedMetadata, typedPayload)
import MessageDb.StreamName ()
import Numeric.Natural (Natural)

newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString)

data Subscription = Subscription
  { subscriberId :: SubscriberId
  , categoryName :: CategoryName
  , messagesPerTick :: Natural
  , positionUpdateIntervalMessages :: Natural
  , tickIntervalMicroseconds :: Natural
  , handlers :: Map MessageType [Message -> IO ()]
  }

subscribe :: SubscriberId -> CategoryName -> Subscription
subscribe subscriberId categoryName =
  Subscription
    { subscriberId = subscriberId
    , categoryName = categoryName
    , messagesPerTick = 100
    , positionUpdateIntervalMessages = 100
    , tickIntervalMicroseconds = 100
    , handlers = Map.empty
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

poll :: Subscription -> IO ()
poll Subscription{..} = do
  threadDelay (fromIntegral tickIntervalMicroseconds)

start :: Subscription -> IO ()
start subscription =
  void $ Immortal.create (\_ -> poll subscription)
