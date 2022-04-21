module MessageDb.Subscribe (
  SubscriberId (..),
  StartPosition (..),
  NumberOfMessages (..),
  Microseconds (..),
  SubscriptionHandlers,
  attachHandler,
  detachHandler,
  emptyHandlers,
  Subscription (..),
  start,
) where

import Control.Concurrent (threadDelay)
import qualified Control.Immortal as Immortal
import Control.Monad (void)
import Data.Aeson (FromJSON)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Functions ()
import MessageDb.Handlers (Handlers, NoState)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (CategoryName, GlobalPosition, Message (Message), MessageType (MessageType))
import MessageDb.StreamName ()
import MessageDb.TypedMessage (TypedMessage)
import Numeric.Natural (Natural)
import qualified MessageDb.Functions as Functions

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

type SubscriptionHandlers = Handlers NoState (IO ())

emptyHandlers :: SubscriptionHandlers
emptyHandlers =
  Handlers.empty

attachHandler :: (FromJSON payload, FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> IO ()) -> SubscriptionHandlers -> SubscriptionHandlers
attachHandler messageType handler =
  Handlers.attach messageType $ \typedMessage _ ->
    handler typedMessage

detachHandler :: MessageType -> SubscriptionHandlers -> SubscriptionHandlers
detachHandler =
  Handlers.detach

data Subscription = Subscription
  { subscriberId :: SubscriberId
  , categoryName :: CategoryName
  , startPosition :: StartPosition
  , messagesPerTick :: NumberOfMessages
  , positionUpdateInterval :: NumberOfMessages
  , tickInterval :: Microseconds
  , logHandler :: NonEmpty Message -> IO ()
  , handlers :: Handlers NoState (IO ())
  }

start :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
start withConnection Subscription{..} = do
  let poll position = do
        messages <- Functions.getCategoryMessages

        threadDelay (fromIntegral (fromMicroseconds tickInterval))

  poll startPosition
