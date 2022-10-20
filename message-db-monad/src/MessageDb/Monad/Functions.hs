module MessageDb.Monad.Functions
  ( lookupById
  , lookupByPosition
  , writeMessage
  , getStreamMessages
  , getCategoryMessages
  , getLastStreamMessage
  , streamVersion
  , writeMessageWithId
  )
where

import Control.Monad.Reader
import qualified Data.Aeson as Aeson
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.Monad (MessageDbData, MonadMessageDb (getMessageDbData))
import qualified MessageDb.Monad as MessageDb
import MessageDb.StreamName (StreamName)
import qualified MessageDb.StreamName as StreamName


getBatchSize :: MessageDbData -> Functions.BatchSize
getBatchSize =
  Functions.FixedSize . MessageDb.batchSize


lookupById :: (MonadIO m, MonadMessageDb m) => Message.MessageId -> m (Maybe Message)
lookupById messageId =
  MessageDb.withConnection $ \connection ->
    Functions.lookupById connection messageId


lookupByPosition :: (MonadIO m, MonadMessageDb m) => Message.GlobalPosition -> m (Maybe Message)
lookupByPosition position =
  MessageDb.withConnection $ \connection ->
    Functions.lookupByPosition connection position


writeMessage
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     , MonadIO m
     , MonadMessageDb m
     )
  => StreamName
  -> Message.MessageType
  -> payload
  -> Maybe metadata
  -> Maybe Functions.ExpectedVersion
  -> m (Message.MessageId, Message.StreamPosition)
writeMessage streamName messageType payload metadata expectedVersion = do
  MessageDb.withConnection $ \connection ->
    Functions.writeMessage connection streamName messageType payload metadata expectedVersion


writeMessageWithId
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     , MonadIO m
     , MonadMessageDb m
     )
  => Message.MessageId
  -> StreamName
  -> Message.MessageType
  -> payload
  -> Maybe metadata
  -> Maybe Functions.ExpectedVersion
  -> m Message.StreamPosition
writeMessageWithId messageId streamName messageType payload metadata expectedVersion = do
  MessageDb.withConnection $ \connection ->
    Functions.writeMessageWithId connection messageId streamName messageType payload metadata expectedVersion


getStreamMessages
  :: ( MonadIO m
     , MonadMessageDb m
     )
  => StreamName
  -> Maybe Message.StreamPosition
  -> Maybe Functions.Condition
  -> m [Message]
getStreamMessages streamName position condition = do
  batchSize <- fmap (Just . getBatchSize) getMessageDbData
  MessageDb.withConnection $ \connection ->
    Functions.getStreamMessages connection streamName position batchSize condition


getCategoryMessages
  :: ( MonadIO m
     , MonadMessageDb m
     )
  => StreamName.Category
  -> Maybe Message.GlobalPosition
  -> Maybe Functions.Correlation
  -> Maybe Functions.ConsumerGroup
  -> Maybe Functions.Condition
  -> m [Message]
getCategoryMessages category position correlation consumerGroup condition = do
  batchSize <- fmap (Just . getBatchSize) getMessageDbData
  MessageDb.withConnection $ \connection ->
    Functions.getCategoryMessages connection category position batchSize correlation consumerGroup condition


getLastStreamMessage :: (MonadIO m, MonadMessageDb m) => StreamName -> m (Maybe Message)
getLastStreamMessage streamName =
  MessageDb.withConnection $ \connection ->
    Functions.getLastStreamMessage connection streamName


streamVersion :: (MonadIO m, MonadMessageDb m) => StreamName -> m (Maybe Message.StreamPosition)
streamVersion streamName =
  MessageDb.withConnection $ \connection ->
    Functions.streamVersion connection streamName
