-- | Subscribe to a category and react to the messages.
module MessageDb.Subscription
  ( Subscription (..),
    subscribe,
    start,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (handleAny)
import Control.Monad (when)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Data.Void (Void)
import MessageDb.Functions ()
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.StreamName (Category)
import qualified MessageDb.Subscription.FailedMessage as FailedMessage
import MessageDb.Subscription.FailureStrategy (FailureStrategy)
import qualified MessageDb.Subscription.FailureStrategy as FailureStrategy
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import MessageDb.Units (Microseconds (..), NumberOfMessages (..))


-- | Defines how to subscribe to a category.
data Subscription = Subscription
  { categoryName :: Category
  , messagesPerTick :: NumberOfMessages
  , tickInterval :: Microseconds
  , logMessages :: NonEmpty Message -> IO ()
  , failureStrategy :: FailureStrategy
  , positionStrategy :: PositionStrategy
  , handlers :: Handlers.SubscriptionHandlers
  , consumerGroup :: Maybe Functions.ConsumerGroup
  , condition :: Maybe Functions.Condition
  , correlation :: Maybe Functions.Correlation
  }


-- | Construct a new subscription.
subscribe :: Category -> Subscription
subscribe categoryName =
  Subscription
    { categoryName = categoryName
    , messagesPerTick = 100
    , tickInterval = 100_000
    , logMessages = \_ -> pure ()
    , failureStrategy = FailureStrategy.ignoreFailures
    , positionStrategy = PositionStrategy.dontSave
    , handlers = Handlers.emptyHandlers
    , consumerGroup = Nothing
    , condition = Nothing
    , correlation = Nothing
    }


-- | Start the subscription. Notice this will never return.
start :: Functions.WithConnection -> Subscription -> IO Void
start withConnection Subscription{..} = do
  let sleep :: IO ()
      sleep =
        threadDelay (fromIntegral (microsecondsToNatural tickInterval))

      queryCategory :: Message.GlobalPosition -> IO [Message]
      queryCategory position =
        withConnection $ \connection ->
          Functions.getCategoryMessages
            connection
            categoryName
            (Just position)
            (Just $ Functions.FixedSize messagesPerTick)
            correlation
            consumerGroup
            condition

      handle :: Message -> IO ()
      handle message =
        let logHandleFailure reason =
              FailureStrategy.logFailure failureStrategy $
                FailedMessage.FailedMessage message (FailedMessage.HandleFailure reason)

            logUnknownFailure exception =
              FailureStrategy.logFailure failureStrategy $
                FailedMessage.FailedMessage message (FailedMessage.UnknownFailure (Text.pack $ show exception))
         in handleAny logUnknownFailure $ do
              result <- Handlers.subscriptionHandle handlers message
              either logHandleFailure pure result

      processMessages :: [Message] -> IO (NumberOfMessages, Maybe Message.GlobalPosition)
      processMessages messages =
        case NonEmpty.nonEmpty messages of
          Nothing -> pure (0, Nothing)
          Just nonEmptyMessages -> do
            logMessages nonEmptyMessages
            traverse_ handle nonEmptyMessages
            pure
              ( NumberOfMessages . fromIntegral $ NonEmpty.length nonEmptyMessages
              , Just . Message.messageGlobalPosition $ NonEmpty.last nonEmptyMessages
              )

      poll :: Message.GlobalPosition -> Message.GlobalPosition -> IO Void
      poll initialPosition lastPositionSaved = do
        messages <- queryCategory initialPosition

        (numberOfMessages, lastMessagePosition) <- processMessages messages

        let currentPosition = fromMaybe initialPosition lastMessagePosition
            nextPosition = maybe initialPosition (+ 1) lastMessagePosition

        positionSaved <-
          PositionStrategy.save
            positionStrategy
            lastPositionSaved
            currentPosition

        when (numberOfMessages < messagesPerTick) sleep

        poll nextPosition (fromMaybe lastPositionSaved positionSaved)

  lastPositionSaved <- PositionStrategy.restore positionStrategy

  poll (lastPositionSaved + 1) lastPositionSaved
