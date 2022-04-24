module MessageDb.Subscription (
  SubscriberId (..),
  StartPosition (..),
  NumberOfMessages (..),
  Microseconds (..),
  Subscription (..),
  subscribe,
  start,
  retryFailures,
) where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (SomeException, handleAny, throwIO, withException)
import Control.Monad (void, when)
import Data.Foldable (traverse_)
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
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.StreamName ()
import MessageDb.Subscription.FailedMessage (failMessage)
import qualified MessageDb.Subscription.FailedMessage as FailedMessage
import MessageDb.Subscription.Handlers (SubscriptionHandlers)
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import Numeric.Natural (Natural)

newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Show, Eq, Ord, Num)

newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Show, Eq, Ord, Num)

newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString, Semigroup)

data StartPosition
  = RestorePosition
  | SpecificPosition Message.GlobalPosition

data Subscription = Subscription
  { subscriberId :: SubscriberId
  , categoryName :: Message.CategoryName
  , startPosition :: StartPosition
  , messagesPerTick :: NumberOfMessages
  , positionUpdateInterval :: NumberOfMessages
  , tickInterval :: Microseconds
  , logMessages :: NonEmpty Message -> IO ()
  , logException :: Message -> SomeException -> IO ()
  , handlers :: SubscriptionHandlers
  , consumerGroup :: Maybe Functions.ConsumerGroup
  , condition :: Maybe Functions.Condition
  , correlation :: Maybe Functions.Correlation
  }

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
    , handlers = SubscriptionHandlers.empty
    }

positionStreamName :: SubscriberId -> Maybe Functions.ConsumerGroup -> Message.StreamName
positionStreamName subscriberId consumerGroup =
  Message.StreamName $
    let groupIdentifier =
          Text.pack $ case consumerGroup of
            Nothing -> ""
            Just Functions.ConsumerGroup{..} ->
              "(" <> show consumerGroupMember <> "," <> show consumerGroupSize <> ")"
     in "_position-" <> fromSubscriberId subscriberId <> groupIdentifier

savePosition :: Postgres.Connection -> SubscriberId -> Maybe Functions.ConsumerGroup -> Message.GlobalPosition -> IO ()
savePosition connection subscriberId consumerGroup position =
  void $
    Functions.writeMessage @Message.GlobalPosition @()
      connection
      (positionStreamName subscriberId consumerGroup)
      "GlobalPositionSaved"
      position
      Nothing
      Nothing

queryStartingPosition :: Postgres.Connection -> SubscriberId -> Maybe Functions.ConsumerGroup -> IO Message.GlobalPosition
queryStartingPosition connection subscriberId consumerGroup = do
  maybeMessage <-
    Functions.getLastStreamMessage connection (positionStreamName subscriberId consumerGroup)

  pure $ case fmap Message.typedPayload maybeMessage of
    Just (Right position) -> position
    _ -> 0

failuresStreamName :: SubscriberId -> Message.CategoryName
failuresStreamName subscriberId =
  Message.category . Message.StreamName $ "_failures_" <> fromSubscriberId subscriberId

saveFailure :: Postgres.Connection -> SubscriberId -> Message -> SomeException -> IO ()
saveFailure connection subscriberId message exception = do
  identifier <-
    case Message.identity (Message.streamName message) of
      Nothing ->
        fmap (Message.StreamName . UUID.toText) UUID.V4.nextRandom
      Just identifier ->
        pure . Message.StreamName $ Message.fromIdentityName identifier

  let streamName =
        let category = Message.StreamName . Message.fromCategoryName $ failuresStreamName subscriberId
         in category <> "-" <> identifier

  void $
    Functions.writeMessage
      connection
      streamName
      "FailedMessage"
      (failMessage message exception)
      (Nothing :: Maybe ())
      Nothing

runSubscription :: (Message -> Message) -> SubscriberId -> (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO Void
runSubscription mapMessage failedSubscriberId withConnection Subscription{..} = do
  let queryCategory :: Message.GlobalPosition -> IO (Maybe (NonEmpty Message))
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
        let logExceptions = handleAny (logException message)

            saveFailedMessage exception =
              withConnection $ \connection ->
                saveFailure connection failedSubscriberId message exception

            handleExceptions action = logExceptions $ withException action saveFailedMessage
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

        (numberOfMessages, lastPosition) <- processMessages (fmap mapMessage <$> messages)

        positionSaved <-
          let position = fromMaybe currentPosition lastPosition
              interval = fromIntegral $ fromNumberOfMessage positionUpdateInterval
              difference = Message.fromGlobalPosition $ position - lastPositionSaved
           in if difference < interval
                then pure Nothing
                else do
                  withConnection $ \connection ->
                    savePosition connection subscriberId consumerGroup position

                  pure $ Just position

        when (numberOfMessages < messagesPerTick) sleep

        poll
          (maybe currentPosition (+ 1) lastPosition)
          (fromMaybe lastPositionSaved positionSaved)

  actualStartingPosition <-
    case startPosition of
      SpecificPosition position -> pure position
      RestorePosition -> withConnection $ \connection ->
        queryStartingPosition connection subscriberId consumerGroup

  poll actualStartingPosition actualStartingPosition

start :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO Void
start withConnection subscription@Subscription{..} =
  runSubscription id subscriberId withConnection subscription

retryFailures :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO Void
retryFailures withConnection subscription@Subscription{..} =
  let fromFailedMessage message =
        case Message.typedPayload message of
          Left _ -> message
          Right failure -> FailedMessage.message failure
   in runSubscription fromFailedMessage subscriberId withConnection $
        subscription
          { subscriberId = "_failures_" <> subscriberId
          , categoryName = failuresStreamName subscriberId
          }
