-- | Provides the functionality to subscribe to Category streams.
module MessageDb.Subscription (
  -- * Subscription Handlers
  SubscriptionHandlers,
  attachHandler,
  detachHandler,
  emptyHandlers,

  -- * Construct and Start a Subscription
  SubscriberId (..),
  StartPosition (..),
  NumberOfMessages (..),
  Microseconds (..),
  Subscription (..),
  subscribe,
  subscribeFailures,
  start,
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, handleAny, onException, throwIO)
import Control.Monad (void, when)
import Data.Aeson (FromJSON)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Data.Void (Void)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Functions ()
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (Handlers, NoState)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import qualified MessageDb.Message as Message
import MessageDb.StreamName ()
import MessageDb.TypedMessage (TypedMessage)
import Numeric.Natural (Natural)

-- | Uniquely identifies this subscriber. This allows us to save the position periodically providing restart capabilities.
newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString, Semigroup)

-- | Defines what global position to start at for subscription.
data StartPosition
  = -- | Use a position that previously saved in message-db. If not found will start at 0.
    RestorePosition
  | -- | Use the position that was specified.
    SpecificPosition Message.GlobalPosition

-- | Unit of measure that equates to a quantity of messages.
newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Show, Eq, Ord, Num)

-- | Unit of measure that equates to microseconds.
newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Show, Eq, Ord, Num)

-- | Handlers for handling different message types.
type SubscriptionHandlers = Handlers NoState (IO ())

-- | An empty set of subscription handlers.
emptyHandlers :: SubscriptionHandlers
emptyHandlers =
  Handlers.empty

-- | Add a handler for a specified message type.
attachHandler :: (FromJSON payload, FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> IO ()) -> SubscriptionHandlers -> SubscriptionHandlers
attachHandler messageType handler =
  Handlers.attach messageType $ \typedMessage _ ->
    handler typedMessage

-- | Remove a handler for a specified message type.
detachHandler :: MessageType -> SubscriptionHandlers -> SubscriptionHandlers
detachHandler =
  Handlers.detach

{- | Position is recorded at stream name: _position-<subscriberId> or _position-<subscriberId(<consumer group member>,<consumer group size>)
     For example with a subscription id of foo then positions will be saved to _position-foo
     or if you specify a consumer group member of 2, and group size of 5 then _position-foo(2,5)
-}
positionStreamName :: SubscriberId -> Maybe Functions.ConsumerGroup -> Message.StreamName
positionStreamName subscriberId consumerGroup =
  Message.StreamName $
    let groupIdentifier =
          Text.pack $ case consumerGroup of
            Nothing -> ""
            Just Functions.ConsumerGroup{..} ->
              "(" <> show consumerGroupMember <> "," <> show consumerGroupSize <> ")"
     in "_position-" <> fromSubscriberId subscriberId <> groupIdentifier

{- | Failures are recorded at category name: _failures_<subscriberId>
   For example with a subscription id of foo then failures will be saved to _failures-foo

   You can subscribe to this category to have automatic retry.
-}
failuresStreamName :: SubscriberId -> Message.CategoryName
failuresStreamName subscriberId =
  Message.category . Message.StreamName $ "_failures_" <> fromSubscriberId subscriberId

-- | A subscription to a category.
data Subscription = Subscription
  { -- | Uniquely identifies a subscriber.
    subscriberId :: SubscriberId
  , -- | Name of the category to subscribe to.
    categoryName :: Message.CategoryName
  , -- | Position to start the subscription at. Defaults to 'RestorePosition'.
    startPosition :: StartPosition
  , -- | Quantity of messages that will be processed in at once. Defaults to 100 messages.
    messagesPerTick :: NumberOfMessages
  , -- | Defines how often the position is saved to message-db. Defaults to 100 messages.
    positionUpdateInterval :: NumberOfMessages
  , -- | Defines how long to wait in between ticks. Defaults to 100,000 microseconds.
    tickInterval :: Microseconds
  , -- | Allows you to log messages each tick. Defaults to '\_ -> pure ()'.
    logMessages :: NonEmpty Message -> IO ()
  , -- | Allows you to log exceptions that occur while calling your handler. Defaults to '\_ _ -> pure ()'.
    logException :: Message -> SomeException -> IO ()
  , -- | Defines how to handle a message per type. Defaults to 'emptyHandlers'.
    handlers :: Handlers NoState (IO ())
  , -- | Allows you to consume this topic in parallel. Defaults to 'Nothing'.
    consumerGroup :: Maybe Functions.ConsumerGroup
  , -- | Allows you to add an extra condition in the WHERE clause when querying for new messages. Defaults to 'Nothing'.
    condition :: Maybe Functions.Condition
  , -- | Allows you to only retrieve messages that only correspond to an origin stream name. Defaults to 'Nothing'.
    correlation :: Maybe Functions.Correlation
  }

-- | Helper function for creating a new subscription.
subscribe :: SubscriberId -> Message.CategoryName -> Subscription
subscribe subscriberId categoryName =
  Subscription
    { subscriberId = subscriberId
    , categoryName = categoryName
    , startPosition = RestorePosition
    , messagesPerTick = 100
    , positionUpdateInterval = 100
    , tickInterval = 100_000
    , logMessages = \_ -> pure ()
    , logException = \_ _ -> pure ()
    , consumerGroup = Nothing
    , condition = Nothing
    , correlation = Nothing
    , handlers = emptyHandlers
    }

subscribeFailures :: SubscriberId -> Subscription
subscribeFailures subscriberId =
  subscribe ("_failures_" <> subscriberId) (failuresStreamName subscriberId)

-- | Start a subscription. Note, you may want to consider using a package like 'immortal' to ensure this stays alive.
start ::
  -- | Safely acquire a postgres connection. Note this is a lot like 'Data.Pool.withResource'.
  (forall a. (Postgres.Connection -> IO a) -> IO a) ->
  -- | The subscription to start.
  Subscription ->
  -- | Returns void because this function will loop infinitely.
  IO Void
start withConnection Subscription{..} = do
  let savePosition :: Message.GlobalPosition -> IO ()
      savePosition position =
        void . withConnection $ \connection ->
          Functions.writeMessage @Message.GlobalPosition @()
            connection
            (positionStreamName subscriberId consumerGroup)
            "GlobalPositionSaved"
            position
            Nothing
            Nothing

      queryStartingPosition :: IO Message.GlobalPosition
      queryStartingPosition = do
        maybeMessage <- withConnection $ \connection ->
          Functions.getLastStreamMessage connection (positionStreamName subscriberId consumerGroup)

        pure $ case fmap Message.typedPayload maybeMessage of
          Just (Right position) -> position
          _ -> 0

      saveFailure :: Message -> IO ()
      saveFailure message = do
        identifier <-
          case Message.identity (Message.streamName message) of
            Nothing ->
              fmap (Message.StreamName . UUID.toText) UUID.V4.nextRandom
            Just identifier ->
              pure . Message.StreamName $ Message.fromIdentityName identifier

        let streamName =
              let category = Message.StreamName . Message.fromCategoryName $ failuresStreamName subscriberId
               in category <> "-" <> identifier

        void . withConnection $ \connection ->
          Functions.writeMessage
            connection
            streamName
            (Message.messageType message)
            (Message.payload message)
            (Just $ Message.metadata message)
            Nothing

      queryCategory :: Message.GlobalPosition -> IO (Maybe (NonEmpty Message))
      queryCategory position =
        fmap NonEmpty.nonEmpty . withConnection $ \connection ->
          Functions.getCategoryMessages
            connection
            categoryName
            (Just position)
            (Just . Functions.FixedSize $ fromNumberOfMessage messagesPerTick)
            correlation
            consumerGroup
            condition

      sleep :: IO ()
      sleep =
        threadDelay (fromIntegral (fromMicroseconds tickInterval))

      handle :: Message -> IO ()
      handle message =
        let handleExceptions = handleAny (logException message) . onException (saveFailure message)
         in handleExceptions $ case Handlers.handle (Message.messageType message) handlers message () of
              Left handleError -> throwIO handleError
              Right effect -> effect

      processMessages :: Maybe (NonEmpty Message) -> IO (NumberOfMessages, Maybe Message.GlobalPosition)
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

      poll :: Message.GlobalPosition -> Message.GlobalPosition -> IO Void
      poll currentPosition lastPositionSaved = do
        messages <- queryCategory currentPosition

        (numberOfMessages, lastPosition) <- processMessages messages

        positionSaved <-
          let position = fromMaybe currentPosition lastPosition
              interval = fromIntegral $ fromNumberOfMessage positionUpdateInterval
              difference = Message.fromGlobalPosition $ position - lastPositionSaved
           in if difference < interval
                then pure Nothing
                else savePosition position $> Just position

        when (numberOfMessages < messagesPerTick) sleep

        poll
          (maybe currentPosition (+ 1) lastPosition)
          (fromMaybe lastPositionSaved positionSaved)

  actualStartingPosition <-
    case startPosition of
      SpecificPosition position -> pure position
      RestorePosition -> queryStartingPosition

  poll actualStartingPosition actualStartingPosition
