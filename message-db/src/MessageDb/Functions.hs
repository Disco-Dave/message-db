{-# LANGUAGE QuasiQuotes #-}

-- | Provides access to the functions described at http://docs.eventide-project.org/user-guide/message-db/server-functions.html
module MessageDb.Functions
  ( lookupById
  , lookupByPosition
  , writeMessage
  , getStreamMessages
  , getCategoryMessages
  , getLastStreamMessage
  , streamVersion
  )
where

import Control.Exception.Safe (handle, throwIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.FromRow as FromRow
import Database.PostgreSQL.Simple.SqlQQ (sql)
import MessageDb.Consumer.BatchSize (BatchSize)
import MessageDb.Consumer.Condition (Condition)
import MessageDb.Consumer.ConsumerGroup (ConsumerGroup, consumerGroupSize, consumerIndex)
import MessageDb.Correlation (Correlation)
import MessageDb.Message (Message (Message), UntypedMessage)
import qualified MessageDb.Message as Message
import MessageDb.Message.GlobalPosition (GlobalPosition)
import MessageDb.Message.MessageId (MessageId)
import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.Message.StreamName.Category (Category)
import MessageDb.Message.StreamPosition (StreamPosition)
import MessageDb.Producer.UnexpectedStreamVersion (parseUnexpectedStreamVersion)
import MessageDb.StreamVersion (StreamVersion)


lookupById :: MonadIO m => Postgres.Connection -> MessageId -> m (Maybe UntypedMessage)
lookupById connection messageId = do
  let query =
        [sql|
          SELECT 
            stream_name
            ,type
            ,position
            ,global_position
            ,data
            ,metadata
            ,time
          FROM message_store.messages
          WHERE id = ?;
        |]

      rowParser = do
        messageStreamName <- FromRow.field
        messageType <- FromRow.field
        messageStreamPosition <- FromRow.field
        messageGlobalPosition <- FromRow.field
        messagePayload <- FromRow.field
        messageMetadata <- FromRow.field
        messageCreatedAt <- FromRow.field
        pure Message{..}

      params = Postgres.Only messageId

  listToMaybe <$> liftIO (Postgres.queryWith rowParser connection query params)


lookupByPosition :: MonadIO m => Postgres.Connection -> GlobalPosition -> m (Maybe UntypedMessage)
lookupByPosition connection globalPosition = do
  let query =
        [sql|
          SELECT 
            id
            ,stream_name
            ,type
            ,position
            ,data
            ,metadata
            ,time
          FROM message_store.messages
          WHERE id = ?;
        |]

      rowParser = do
        messageId <- FromRow.field
        messageStreamName <- FromRow.field
        messageType <- FromRow.field
        messageStreamPosition <- FromRow.field
        messagePayload <- FromRow.field
        messageMetadata <- FromRow.field
        messageCreatedAt <- FromRow.field
        pure
          Message
            { messageGlobalPosition = globalPosition
            , ..
            }

      params = Postgres.Only globalPosition

  listToMaybe <$> liftIO (Postgres.queryWith rowParser connection query params)


writeMessage
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     , MonadIO m
     , MonadFail m
     )
  => Postgres.Connection
  -> MessageId
  -> StreamName
  -> MessageType
  -> payload
  -> Maybe metadata
  -> Maybe StreamVersion
  -> m StreamPosition
writeMessage connection messageId streamName messageType payload metadata expectedVersion = do
  let query =
        [sql|
          SELECT message_store.write_message (
            id => ?
            ,stream_name => ?
            ,type => ?
            ,data => ?
            ,metadata => ?
            ,expected_version => ?
          );
        |]

      params =
        ( messageId
        , streamName
        , messageType
        , Aeson.toJSON payload
        , fmap Aeson.toJSON metadata
        , expectedVersion
        )

      handleSqlError sqlError =
        case parseUnexpectedStreamVersion sqlError of
          Nothing ->
            throwIO sqlError
          Just expectedVersionViolation ->
            throwIO expectedVersionViolation

  [Postgres.Only position] <-
    liftIO . handle handleSqlError $
      Postgres.query connection query params

  pure position


-- | Retrieve messages from a single stream, optionally specifying the starting position, the number of messages to retrieve, and an additional condition that will be appended to the SQL command's WHERE clause.
getStreamMessages
  :: ( MonadIO m
     )
  => Postgres.Connection
  -> StreamName
  -> Maybe StreamPosition
  -> Maybe BatchSize
  -> Maybe Condition
  -> m [UntypedMessage]
getStreamMessages connection streamName position batchSize condition = do
  let query =
        [sql|
          SELECT 
            id
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data
            ,metadata
            ,time
          FROM message_store.get_stream_messages (
            stream_name => ?
            ,"position" => ?
            ,batch_size => ?
            ,condition => ?
          );
        |]

      params =
        ( streamName
        , position
        , batchSize
        , condition
        )

      rowParser = do
        messageId <- FromRow.field
        messageStreamName <- FromRow.field
        messageType <- FromRow.field
        messageStreamPosition <- FromRow.field
        messageGlobalPosition <- FromRow.field
        messagePayload <- FromRow.field
        messageMetadata <- FromRow.field
        messageCreatedAt <- FromRow.field
        pure Message{..}

  liftIO $ Postgres.queryWith rowParser connection query params


-- | Retrieve messages from a category of streams, optionally specifying the starting position, the number of messages to retrieve, the correlation category for Pub/Sub, consumer group parameters, and an additional condition that will be appended to the SQL command's WHERE clause.
getCategoryMessages
  :: ( MonadIO m
     )
  => Postgres.Connection
  -> Category
  -> Maybe GlobalPosition
  -> Maybe BatchSize
  -> Maybe Correlation
  -> Maybe ConsumerGroup
  -> Maybe Condition
  -> m [UntypedMessage]
getCategoryMessages connection category position batchSize correlation consumerGroup condition = do
  let query =
        [sql|
          SELECT 
            id
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data
            ,metadata
            ,time
          FROM message_store.get_category_messages (
            category => ?
            ,"position" => ?
            ,batch_size => ?
            ,correlation => ?
            ,consumer_group_member => ?
            ,consumer_group_size => ?
            ,condition => ?
          );
        |]

      params =
        ( category
        , position
        , batchSize
        , correlation
        , fmap consumerIndex consumerGroup
        , fmap consumerGroupSize consumerGroup
        , condition
        )

      rowParser = do
        messageId <- FromRow.field
        messageStreamName <- FromRow.field
        messageType <- FromRow.field
        messageStreamPosition <- FromRow.field
        messageGlobalPosition <- FromRow.field
        messagePayload <- FromRow.field
        messageMetadata <- FromRow.field
        messageCreatedAt <- FromRow.field
        pure Message{..}

  liftIO $ Postgres.queryWith rowParser connection query params


-- | Row from the messages table that corresponds to the highest position number in the stream.
getLastStreamMessage
  :: ( MonadIO m
     )
  => Postgres.Connection
  -> StreamName
  -> m (Maybe UntypedMessage)
getLastStreamMessage connection streamName = do
  let query =
        [sql|
          SELECT 
            id
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data
            ,metadata
            ,time
          FROM message_store.get_last_stream_message (
            stream_name => ?
          );
        |]

      params =
        Postgres.Only streamName

      rowParser = do
        messageId <- FromRow.field
        messageStreamName <- FromRow.field
        messageType <- FromRow.field
        messageStreamPosition <- FromRow.field
        messageGlobalPosition <- FromRow.field
        messagePayload <- FromRow.field
        messageMetadata <- FromRow.field
        messageCreatedAt <- FromRow.field
        pure Message{..}

  liftIO $ listToMaybe <$> Postgres.queryWith rowParser connection query params


-- | Highest position number in the stream.
streamVersion
  :: (MonadIO m)
  => Postgres.Connection
  -> StreamName
  -> m (Maybe StreamPosition)
streamVersion connection streamName = do
  let query =
        [sql|
          SELECT message_store.stream_version (
            stream_name => ?
          );
        |]

      params =
        Postgres.Only streamName

  result <- liftIO $ Postgres.query connection query params

  pure $ case result of
    [Postgres.Only (Just position)] -> Just position
    _ -> Nothing
