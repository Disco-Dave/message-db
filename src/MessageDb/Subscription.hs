-- | Subscribe to a category and react to the messages.
module MessageDb.Subscription
  ( Subscription (..),
    subscribe,
    start,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (tryAny)
import Control.Monad (when)
import Data.Bifunctor (first)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import MessageDb.Functions ()
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.StreamName (CategoryName)
import MessageDb.Subscription.FailureStrategy (FailureStrategy)
import qualified MessageDb.Subscription.FailureStrategy as FailureStrategy
import MessageDb.Subscription.Handlers (SubscriptionHandlers)
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import MessageDb.Units (Microseconds (..), NumberOfMessages (..))


-- | Defines how to subscribe to a category.
data Subscription = Subscription
  { categoryName :: CategoryName
  , messagesPerTick :: NumberOfMessages
  , tickInterval :: Microseconds
  , logMessages :: NonEmpty Message -> IO ()
  , failureStrategy :: FailureStrategy
  , positionStrategy :: PositionStrategy
  , handlers :: SubscriptionHandlers
  , consumerGroup :: Maybe Functions.ConsumerGroup
  , condition :: Maybe Functions.Condition
  , correlation :: Maybe Functions.Correlation
  }


-- | Construct a new subscription.
subscribe :: CategoryName -> Subscription
subscribe categoryName =
  Subscription
    { categoryName = categoryName
    , messagesPerTick = 100
    , tickInterval = 100_000
    , logMessages = \_ -> pure ()
    , failureStrategy = FailureStrategy.ignoreFailures
    , positionStrategy = PositionStrategy.dontSave
    , handlers = SubscriptionHandlers.empty
    , consumerGroup = Nothing
    , condition = Nothing
    , correlation = Nothing
    }


-- | Start the subscription. Notice this will never return.
start :: Functions.WithConnection -> Subscription -> IO Void
start withConnection Subscription{..} = do
  let sleep :: IO ()
      sleep =
        threadDelay (fromIntegral (fromMicroseconds tickInterval))

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
      handle message = do
        result <-
          case SubscriptionHandlers.handle handlers message of
            Left err ->
              pure . Left $ FailureStrategy.HandleFailure err
            Right effect ->
              first FailureStrategy.UnknownFailure <$> tryAny effect

        case result of
          Right () ->
            pure ()
          Left reason ->
            FailureStrategy.logFailure failureStrategy message reason

      processMessages :: [Message] -> IO (NumberOfMessages, Maybe Message.GlobalPosition)
      processMessages messages =
        case NonEmpty.nonEmpty messages of
          Nothing -> pure (0, Nothing)
          Just nonEmptyMessages -> do
            logMessages nonEmptyMessages
            traverse_ handle nonEmptyMessages
            pure
              ( NumberOfMessages . fromIntegral $ NonEmpty.length nonEmptyMessages
              , Just . Message.globalPosition $ NonEmpty.last nonEmptyMessages
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
