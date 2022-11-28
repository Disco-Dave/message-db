module MessageDb.Consumer.Subscription
  ( SubscriptionPosition (..)
  , Subscription (..)
  , subscribe
  , startSubscription
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Safe (MonadCatch, handleAny)
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (traverse_)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import Data.Void (Void)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Consumer.BatchSize as BatchSize
import MessageDb.Consumer.Condition (Condition)
import MessageDb.Consumer.Subscription.ConsumerGroup (ConsumerGroup)
import MessageDb.Consumer.Subscription.Correlation (Correlation)
import MessageDb.Consumer.Subscription.Error (SubscriptionError (..))
import MessageDb.Consumer.Subscription.ErrorReason (SubscriptionErrorReason (..))
import MessageDb.Consumer.Subscription.Handlers (SubscriptionHandlers, handleSubscription)
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message (..), UntypedMessage)
import MessageDb.Message.GlobalPosition (GlobalPosition)
import MessageDb.Message.StreamName.Category (Category)
import MessageDb.Units.Microseconds (Microseconds (microsecondsToNatural))
import MessageDb.Units.NumberOfMessages (NumberOfMessages (..))


data SubscriptionPosition m = SubscriptionPosition
  { restorePosition :: m (Maybe GlobalPosition)
  , savePosition :: GlobalPosition -> m ()
  }


-- | Defines how to subscribe to a category.
data Subscription m = Subscription
  { subConnectionPool :: Pool Postgres.Connection
  , subCategory :: Category
  , subBatchSize :: NumberOfMessages
  , subTickInterval :: Microseconds
  , subConsumerGroup :: Maybe ConsumerGroup
  , subCondition :: Maybe Condition
  , subCorrelation :: Maybe Correlation
  , subPosition :: SubscriptionPosition m
  , subOnError :: SubscriptionError -> m ()
  , subHandlers :: SubscriptionHandlers m
  }


subscribe :: Applicative m => Pool Postgres.Connection -> Category -> SubscriptionHandlers m -> Subscription m
subscribe connectionPool category handlers =
  Subscription
    { subConnectionPool = connectionPool
    , subCategory = category
    , subHandlers = handlers
    , subBatchSize = 100
    , subTickInterval = 500_000
    , subConsumerGroup = Nothing
    , subCondition = Nothing
    , subCorrelation = Nothing
    , subOnError = \_ -> pure ()
    , subPosition =
        SubscriptionPosition
          { restorePosition = pure Nothing
          , savePosition = \_ -> pure ()
          }
    }


startSubscription :: forall m. (MonadIO m, MonadCatch m) => Subscription m -> m Void
startSubscription Subscription{..} = do
  let sleep :: m ()
      sleep =
        liftIO . threadDelay . fromIntegral $
          microsecondsToNatural subTickInterval

      queryCategory :: GlobalPosition -> m [UntypedMessage]
      queryCategory globalPosition =
        liftIO . Pool.withResource subConnectionPool $ \connection ->
          Functions.getCategoryMessages
            connection
            subCategory
            (Just globalPosition)
            (Just $ BatchSize.FixedSize subBatchSize)
            subCorrelation
            subConsumerGroup
            subCondition

      handle :: UntypedMessage -> m ()
      handle message = do
        let onError reason =
              subOnError
                SubscriptionError
                  { subscriptionErrorReason = reason
                  , subscriptionErrorMessage = message
                  }
         in case handleSubscription subHandlers message of
              Left handlerError ->
                onError $ SubscriptionHandlerError handlerError
              Right dangerousAction -> do
                handleAny (onError . SubscriptionException) dangerousAction

      processMessages :: [UntypedMessage] -> m (NumberOfMessages, Maybe GlobalPosition)
      processMessages messages =
        case NonEmpty.nonEmpty messages of
          Nothing -> pure (0, Nothing)
          Just nonEmptyMessages -> do
            traverse_ handle nonEmptyMessages
            pure
              ( NumberOfMessages . fromIntegral $ NonEmpty.length nonEmptyMessages
              , Just . messageGlobalPosition $ NonEmpty.last nonEmptyMessages
              )

      poll :: GlobalPosition -> m Void
      poll !initialPosition = do
        (messagesProcessed, lastMessagePosition) <-
          processMessages =<< queryCategory initialPosition

        when (messagesProcessed > 0) $ do
          let currentPosition = fromMaybe initialPosition lastMessagePosition
           in savePosition subPosition currentPosition

          when (messagesProcessed < subBatchSize) sleep

        let nextPosition = maybe initialPosition (+ 1) lastMessagePosition
         in poll nextPosition

  lastPositionSaved <- restorePosition subPosition

  poll (fromMaybe 0 lastPositionSaved)
