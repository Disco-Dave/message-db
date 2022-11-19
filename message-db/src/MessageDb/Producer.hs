module MessageDb.Producer
  ( produceWithId
  , produce
  )
where

import Control.Monad.IO.Class (MonadIO)
import qualified Data.Aeson as Aeson
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import MessageDb.Message.MessageId (MessageId (..), newMessageId)
import MessageDb.Message.StreamPosition (StreamPosition (..))
import MessageDb.Producer.ProduceRecord (ProduceRecord (..))


produceWithId
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     , MonadIO m
     , MonadFail m
     )
  => Postgres.Connection
  -> MessageId
  -> ProduceRecord payload metadata
  -> m StreamPosition
produceWithId connection messageId ProduceRecord{..} =
  Functions.writeMessage
    connection
    messageId
    produceStreamName
    produceMessageType
    producePayload
    produceMetadata
    produceExpectedVersion


produce
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     , MonadIO m
     , MonadFail m
     )
  => Postgres.Connection
  -> ProduceRecord payload metadata
  -> m (MessageId, StreamPosition)
produce connection record = do
  messageId <- newMessageId
  position <- produceWithId connection messageId record
  pure (messageId, position)
