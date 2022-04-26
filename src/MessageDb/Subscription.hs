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
import MessageDb.StreamName ()
import MessageDb.Subscription.FailureStrategy (FailureStrategy)
import qualified MessageDb.Subscription.FailureStrategy as FailureStrategy
import MessageDb.Subscription.Handlers (SubscriptionHandlers)
import qualified MessageDb.Subscription.Handlers as SubscriptionHandlers
import MessageDb.Subscription.PositionStrategy (PositionStrategy)
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import MessageDb.Units (Microseconds (..), NumberOfMessages (..))


data Subscription = Subscription
  { categoryName :: Message.CategoryName
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


subscribe :: Message.CategoryName -> Subscription
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


start :: Functions.WithConnection -> Subscription -> IO Void
start withConnection Subscription{..} = do
  let sleep :: IO ()
      sleep =
        threadDelay (fromIntegral (fromMicroseconds tickInterval))

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

      handle :: Message -> IO ()
      handle message = do
        result <-
          case SubscriptionHandlers.handle (Message.messageType message) handlers message of
            Left err ->
              pure . Left $ FailureStrategy.HandleFailure err
            Right effect ->
              first FailureStrategy.UnknownFailure <$> tryAny effect

        case result of
          Right () ->
            pure ()
          Left reason ->
            FailureStrategy.logFailure failureStrategy message reason

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
      poll initialPosition lastPositionSaved = do
        messages <- queryCategory initialPosition

        (numberOfMessages, currentPosition) <- processMessages messages

        positionSaved <-
          PositionStrategy.save
            positionStrategy
            lastPositionSaved
            (fromMaybe initialPosition currentPosition)

        when (numberOfMessages < messagesPerTick) sleep

        poll
          (maybe initialPosition (+ 1) currentPosition)
          (fromMaybe lastPositionSaved positionSaved)

  startingPosition <- PositionStrategy.restore positionStrategy

  poll startingPosition startingPosition
