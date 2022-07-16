module MessageDb.Subscription.Monad
  ( subscriptionHandler
  , listToHandlers
  , makePositionStrategy
  , makeFailureStrategy
  , Settings (..)
  , defaultSettings
  , start
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson.Types as Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Pool as Pool
import Data.Traversable (for)
import Data.Void (Void)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import qualified MessageDb.Message as Message
import MessageDb.Monad (MessageDbData (..), MonadMessageDb (getMessageDbData))
import qualified MessageDb.StreamName as StreamName
import qualified MessageDb.Subscription as Subscription
import MessageDb.Subscription.FailedMessage (FailedMessage)
import MessageDb.Subscription.FailureStrategy (FailureStrategy (FailureStrategy))
import qualified MessageDb.Subscription.FailureStrategy as FailureStrategy
import MessageDb.Subscription.PositionStrategy (PositionStrategy (PositionStrategy))
import qualified MessageDb.Subscription.PositionStrategy as PositionStrategy
import UnliftIO (MonadUnliftIO (withRunInIO))


subscriptionHandler ::
  forall payload metadata m.
  ( Aeson.FromJSON payload
  , Aeson.FromJSON metadata
  , MonadUnliftIO m
  ) =>
  (Message -> payload -> metadata -> m ()) ->
  m Handlers.SubscriptionHandler
subscriptionHandler handler =
  withRunInIO $ \runInIO ->
    pure (Handlers.subscriptionHandler (\msg pay meta -> runInIO $ handler msg pay meta))


listToHandlers :: Applicative m => [(MessageType, m Handlers.SubscriptionHandler)] -> m Handlers.SubscriptionHandlers
listToHandlers list =
  fmap Handlers.listToHandlers . for list $ \(messageType, handler) ->
    fmap (messageType,) handler


makePositionStrategy ::
  MonadUnliftIO m =>
  m Message.GlobalPosition ->
  (PositionStrategy.LastPositionSaved -> PositionStrategy.CurrentPosition -> m (Maybe PositionStrategy.PositionSaved)) ->
  m PositionStrategy
makePositionStrategy restore save =
  withRunInIO $ \runInIO ->
    pure $
      PositionStrategy
        { PositionStrategy.restore = runInIO restore
        , PositionStrategy.save = \lps cp -> runInIO (save lps cp)
        }


makeFailureStrategy :: MonadUnliftIO m => (FailedMessage -> m ()) -> m FailureStrategy
makeFailureStrategy logFailure =
  withRunInIO $ \runInIO ->
    pure $ FailureStrategy (runInIO . logFailure)


data Settings m = Settings
  { logMessages :: NonEmpty Message -> m ()
  , failureStrategy :: m FailureStrategy
  , positionStrategy :: m PositionStrategy
  }


defaultSettings :: Applicative m => Settings m
defaultSettings =
  Settings
    { logMessages = \_ -> pure ()
    , failureStrategy = pure FailureStrategy.ignoreFailures
    , positionStrategy = pure PositionStrategy.dontSave
    }


start :: (MonadMessageDb m, MonadUnliftIO m) => Settings m -> StreamName.Category -> m Handlers.SubscriptionHandlers -> m Void
start settings categoryName handlers = do
  MessageDbData{..} <- getMessageDbData

  unwrappedHandlers <- handlers

  unwrappedLogMessages <-
    withRunInIO $ \runInIO ->
      pure (runInIO . logMessages settings)

  unwrappedFailureStrategy <-
    failureStrategy settings

  unwrappedPositionStrategy <-
    positionStrategy settings

  let subscription =
        (Subscription.subscribe categoryName)
          { Subscription.batchSize = batchSize
          , Subscription.tickInterval = pollInterval
          , Subscription.failureStrategy = unwrappedFailureStrategy
          , Subscription.positionStrategy = unwrappedPositionStrategy
          , Subscription.handlers = unwrappedHandlers
          , Subscription.consumerGroup = consumerGroup
          , Subscription.logMessages = unwrappedLogMessages
          }

  liftIO $ Subscription.start (Pool.withResource connectionPool) subscription
