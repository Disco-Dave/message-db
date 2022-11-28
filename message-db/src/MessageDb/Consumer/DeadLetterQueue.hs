module MessageDb.Consumer.DeadLetterQueue
  ( writeToDlq
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Consumer.Subscription.Error (SubscriptionError)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.Producer (produce)
import MessageDb.Producer.ProduceRecord (ProduceRecord (..), emptyProduceRecord)


writeToDlq
  :: (MonadIO m)
  => Pool Postgres.Connection
  -> (SubscriptionError -> Bool)
  -> StreamName
  -> SubscriptionError
  -> m ()
writeToDlq connectionPool shouldWriteToDlq dlqStreamName subscriptionError =
  when (shouldWriteToDlq subscriptionError) $ do
    let record =
          emptyProduceRecord
            { produceStreamName = dlqStreamName
            , produceMessageType = "SubscriptionError"
            , producePayload = subscriptionError
            }

    void . liftIO . Pool.withResource connectionPool $ \connection ->
      produce connection record
