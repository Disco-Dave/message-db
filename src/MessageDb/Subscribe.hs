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
  subscribe,
  start,
) where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, throwIO)
import Control.Exception.Safe (handleAny)
import qualified Control.Immortal as Immortal
import Control.Monad (void, when)
import Data.Aeson (FromJSON)
import Data.Foldable (traverse_)
import Data.Function ((&))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Functions ()
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (Handlers, NoState)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (CategoryName, GlobalPosition, Message (Message), MessageType (MessageType))
import qualified MessageDb.Message as Message
import MessageDb.StreamName ()
import MessageDb.TypedMessage (TypedMessage)
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
  , logMessages :: NonEmpty Message -> IO ()
  , logException :: Message -> SomeException -> IO ()
  , handlers :: Handlers NoState (IO ())
  , consumerGroup :: Maybe Functions.ConsumerGroup
  , condition :: Maybe Functions.Condition
  , correlation :: Maybe Functions.Correlation
  }

subscribe :: SubscriberId -> CategoryName -> Subscription
subscribe subscriberId categoryName =
  Subscription
    { subscriberId = subscriberId
    , categoryName = categoryName
    , startPosition = RestorePosition
    , messagesPerTick = 100
    , positionUpdateInterval = 100
    , tickInterval = 1_00_000
    , logMessages = \_ -> pure ()
    , logException = \_ _ -> pure ()
    , consumerGroup = Nothing
    , condition = Nothing
    , correlation = Nothing
    , handlers = emptyHandlers
    }

start :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
start withConnection Subscription{..} = do
  let handle message =
        handleAny (logException message) $ case Handlers.handle (Message.messageType message) handlers message () of
          Left handleError -> throwIO handleError
          Right effect -> effect

      sleep =
        threadDelay (fromIntegral (fromMicroseconds tickInterval))

      queryCategory position =
        withConnection $ \connection ->
          NonEmpty.nonEmpty
            <$> Functions.getCategoryMessages
              connection
              categoryName
              (Just position)
              (Just . Functions.FixedSize $ fromNumberOfMessage messagesPerTick)
              correlation
              consumerGroup
              condition

      processMessages maybeMessages =
        case maybeMessages of
          Nothing -> pure (0, Nothing)
          Just messages -> do
            logMessages messages
            traverse_ handle messages
            pure
              ( NumberOfMessages . fromIntegral $ NonEmpty.length messages
              , Just . Message.globalPosition $ NonEmpty.last messages
              )

      poll position = do
        messages <- queryCategory position
        (numberOfMessages, lastPosition) <- processMessages messages
        -- TODO Save position

        when (numberOfMessages < messagesPerTick) sleep

        poll $ fromMaybe 0 lastPosition
   in poll 0
