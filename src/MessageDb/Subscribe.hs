module MessageDb.Subscribe (
  SubscriberId (..),
  StartPosition (..),
  NumberOfMessages (..),
  Microseconds (..),
  Subscription (..),
  subscribe,
  ParseException (..),
  typedHandler,
  registerAnything,
  register,
  start,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (Exception, throwIO)
import qualified Control.Immortal as Immortal
import Control.Monad (void)
import Data.Aeson (FromJSON)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Functions ()
import MessageDb.Message (CategoryName, GlobalPosition (GlobalPosition), Message, MessageType, typedMetadata, typedPayload)
import MessageDb.StreamName ()
import Numeric.Natural (Natural)

newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString)

data StartPosition
  = RestorePosition
  | SpecificPosition GlobalPosition

newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Show, Eq, Ord, Num)

newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Show, Eq, Ord, Num)

data Subscription = Subscription
  { subscriberId :: SubscriberId
  , categoryName :: CategoryName
  , startPosition :: StartPosition
  , messagesPerTick :: NumberOfMessages
  , positionUpdateInterval :: NumberOfMessages
  , tickInterval :: Microseconds
  , handlers :: Map MessageType [Message -> IO ()]
  }

subscribe :: SubscriberId -> CategoryName -> Subscription
subscribe subscriberId categoryName =
  Subscription
    { subscriberId = subscriberId
    , categoryName = categoryName
    , startPosition = RestorePosition
    , messagesPerTick = 100
    , positionUpdateInterval = 100
    , tickInterval = 100
    , handlers = Map.empty
    }

typedHandler :: (FromJSON payload, FromJSON metadata) => (payload -> metadata -> IO ()) -> Message -> IO ()
typedHandler untypedHandler message = do
  let parseOrThrow = either (throwIO . ParseException) pure
  payload <- parseOrThrow $ typedPayload message
  metadata <- parseOrThrow $ typedMetadata message
  untypedHandler payload metadata

newtype ParseException = ParseException String deriving (Show)
instance Exception ParseException

registerAnything :: MessageType -> (Message -> IO ()) -> Subscription -> Subscription
registerAnything messageType handle subscription =
  let addHandler oldHandlers =
        Just $ handle : fromMaybe [] oldHandlers
   in subscription
        { handlers = Map.alter addHandler messageType (handlers subscription)
        }

register :: (FromJSON payload, FromJSON metadata) => MessageType -> (payload -> metadata -> IO ()) -> Subscription -> Subscription
register messageType handler =
  registerAnything messageType (typedHandler handler)

poll :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
poll withConnection Subscription{..} = do
  threadDelay (fromIntegral (fromMicroseconds tickInterval))

start :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
start withConnection subscription =
  void . Immortal.create $ \_ ->
    poll withConnection subscription
