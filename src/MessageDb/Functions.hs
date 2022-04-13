{-# LANGUAGE QuasiQuotes #-}

-- | Provides access to the functions described at http://docs.eventide-project.org/user-guide/message-db/server-functions.html
module MessageDb.Functions (
  ExpectedVersion (..),
  BatchSize (..),
  Condition (..),
  ConsumerGroup (..),
  writeMessage,
  getStreamMessages,
  getCategoryMessages,
  getLastStreamMessage,
  streamVersion,
) where

import qualified Data.Aeson as Aeson
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple as Postgres
import Database.PostgreSQL.Simple.FromRow (RowParser, field)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import MessageDb.Message (Message (Message))
import qualified MessageDb.Message as Message
import Numeric.Natural (Natural)

newtype ExpectedVersion = ExpectedVersion
  { fromExpectedVersion :: Message.StreamPosition
  }
  deriving (Show, Eq, Ord)

data ConsumerGroup = ConsumerGroup
  { consumerGroupMember :: Natural
  , consumerGroupSize :: Natural
  }
  deriving (Show, Eq)

newtype Condition = Condition
  { fromCondition :: Text
  }
  deriving (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON, IsString)

newtype Correlation = Correlation
  { fromCorrelation :: Text
  }
  deriving (Show, Eq, Ord, Aeson.ToJSON, Aeson.FromJSON, IsString)

data BatchSize
  = FixedSize Natural
  | Unlimited
  deriving (Show, Eq)

batchSizeToInteger :: BatchSize -> Integer
batchSizeToInteger batchSize =
  case batchSize of
    FixedSize size -> toInteger size
    Unlimited -> -1

messageParser :: RowParser Message
messageParser = do
  messageId <- field
  streamName <- field
  messageType <- field
  streamPosition <- field
  globalPosition <- field
  payload <- field
  metadata <- field
  createdAtTimestamp <- field
  pure Message{..}

-- | Write a JSON-formatted message to a named stream, optionally specifying JSON-formatted metadata and an expected version number.
writeMessage ::
  ( Aeson.ToJSON payload
  , Aeson.ToJSON metadata
  ) =>
  Postgres.Connection ->
  Message.StreamName ->
  Message.MessageType ->
  payload ->
  Maybe metadata ->
  Maybe ExpectedVersion ->
  IO (Message.MessageId, Message.StreamPosition)
writeMessage connection streamName messageType payload metadata expectedVersion = do
  -- TODO Throw specific exception when expected version check fails
  messageId <- Message.newMessageId

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
        ( UUID.toText $ Message.fromMessageId messageId
        , Message.fromStreamName streamName
        , Message.fromMessageType messageType
        , Aeson.toJSON payload
        , fmap Aeson.toJSON metadata
        , fmap (Message.fromStreamPosition . fromExpectedVersion) expectedVersion
        )

  [Postgres.Only position] <-
    Postgres.query connection query params

  pure (messageId, Message.StreamPosition position)

-- | Retrieve messages from a single stream, optionally specifying the starting position, the number of messages to retrieve, and an additional condition that will be appended to the SQL command's WHERE clause.
getStreamMessages ::
  Postgres.Connection ->
  Message.StreamName ->
  Maybe Message.StreamPosition ->
  Maybe BatchSize ->
  Maybe Condition ->
  IO [Message]
getStreamMessages connection streamName position batchSize condition =
  let query =
        [sql|
          SELECT 
            id::uuid
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data::jsonb
            ,metadata::jsonb
            ,time::timestamptz
          FROM message_store.get_stream_messages (
            stream_name => ?
            ,position => ?
            ,batch_size => ?
            ,condition => ?
          );
        |]
      params =
        ( Message.fromStreamName streamName
        , maybe 0 Message.fromStreamPosition position
        , maybe 1000 batchSizeToInteger batchSize
        , fmap fromCondition condition
        )
   in Postgres.queryWith messageParser connection query params

-- | Retrieve messages from a category of streams, optionally specifying the starting position, the number of messages to retrieve, the correlation category for Pub/Sub, consumer group parameters, and an additional condition that will be appended to the SQL command's WHERE clause.
getCategoryMessages ::
  Postgres.Connection ->
  Message.CategoryName ->
  Maybe Message.GlobalPosition ->
  Maybe BatchSize ->
  Maybe Correlation ->
  Maybe ConsumerGroup ->
  Maybe Condition ->
  IO [Message]
getCategoryMessages connection category position batchSize correlation consumerGroup condition =
  let query =
        [sql|
          SELECT 
            id::uuid
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data::jsonb
            ,metadata::jsonb
            ,time::timestamptz
          FROM message_store.get_category_messages (
            category_name => ?
            ,position => ?
            ,batch_size => ?
            ,correlation => ?
            ,consumer_group_member => ?
            ,consumer_group_size => ?
            ,condition => ?
          );
        |]
      params =
        ( Message.fromCategoryName category
        , maybe 0 Message.fromGlobalPosition position
        , maybe 1000 batchSizeToInteger batchSize
        , fmap fromCorrelation correlation
        , fmap (toInteger . consumerGroupMember) consumerGroup
        , fmap (toInteger . consumerGroupSize) consumerGroup
        , fmap fromCondition condition
        )
   in Postgres.queryWith messageParser connection query params

-- | Row from the messages table that corresponds to the highest position number in the stream.
getLastStreamMessage :: Postgres.Connection -> Message.StreamName -> IO (Maybe Message)
getLastStreamMessage connection streamName =
  let query =
        [sql|
          SELECT 
            id::uuid
            ,stream_name
            ,type
            ,position
            ,global_position
            ,data::jsonb
            ,metadata::jsonb
            ,time::timestamptz
          FROM message_store.get_last_stream_message (
            stream_name => ?
          );
        |]
      params =
        Postgres.Only (Message.fromStreamName streamName)
   in listToMaybe <$> Postgres.queryWith messageParser connection query params

-- | Highest position number in the stream.
streamVersion :: Postgres.Connection -> Message.StreamName -> IO (Maybe Message.StreamPosition)
streamVersion connection streamName = do
  let query =
        [sql|
          SELECT message_store.stream_version (
            stream_name => ?
          );
        |]
      params =
        Postgres.Only (Message.fromStreamName streamName)

  result <- Postgres.query connection query params

  pure $ case result of
    [Postgres.Only position] -> Just $ Message.StreamPosition position
    _ -> Nothing
