{-# LANGUAGE QuasiQuotes #-}

-- | Provides access to the functions described at http://docs.eventide-project.org/user-guide/message-db/server-functions.html
module MessageDb.Functions
  ( WithConnection
  , BatchSize (..)
  , Condition (..)
  , ConsumerGroup (..)
  , Correlation (..)
  , StreamVersion (..)
  , ExpectedVersion (..)
  , ExpectedVersionViolation (..)
  , parseExpectedVersionViolation
  , lookupById
  , lookupByPosition
  , writeMessageWithId
  , writeMessageWithId_
  , writeMessage
  , writeMessage_
  , getStreamMessages
  , getCategoryMessages
  , getLastStreamMessage
  , streamVersion
  )
where

import Control.Exception (Exception, handle, throwIO)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as Char8
import Data.Coerce (Coercible, coerce)
import Data.Maybe (listToMaybe)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.FromRow (RowParser, field, fieldWith)
import Database.PostgreSQL.Simple.SqlQQ (sql)
import MessageDb.Message (Message (Message))
import qualified MessageDb.Message as Message
import MessageDb.StreamName (Category, StreamName (StreamName), categoryToText, streamNameToText)
import MessageDb.Units (NumberOfMessages)
import Numeric.Natural (Natural)


type WithConnection = forall records. (Postgres.Connection -> IO records) -> IO records


data ConsumerGroup = ConsumerGroup
  { consumerGroupMember :: Natural
  , consumerGroupSize :: Natural
  }
  deriving (Show, Eq)


newtype Condition = Condition
  { conditionToText :: Text
  }
  deriving (Eq, Ord, Aeson.ToJSON, Aeson.FromJSON, IsString)
  deriving (Show) via Text


newtype Correlation = Correlation
  { correlationToText :: Text
  }
  deriving (Eq, Ord, Aeson.ToJSON, Aeson.FromJSON, IsString)
  deriving (Show) via Text


data StreamVersion
  = DoesNotExist
  | DoesExist Message.StreamPosition
  deriving (Show, Eq, Ord)


newtype ExpectedVersion = ExpectedVersion
  { expectedToStreamVersion :: StreamVersion
  }
  deriving (Show, Eq, Ord)


versionToInteger :: Coercible a StreamVersion => a -> Integer
versionToInteger version =
  case coerce version of
    DoesNotExist -> -1
    DoesExist position -> toInteger position


newtype ExpectedVersionViolation = ExpectedVersionViolation
  { expectedVersionViolationToSqlError :: Postgres.SqlError
  }
  deriving (Show, Eq)
instance Exception ExpectedVersionViolation


parseExpectedVersionViolation :: Postgres.SqlError -> Maybe ExpectedVersionViolation
parseExpectedVersionViolation sqlError@Postgres.SqlError{..} =
  {- Example error of what we are looking for

        SqlError
          { sqlState = "P0001"
          , sqlExecStatus = FatalError
          , sqlErrorMsg = "Wrong expected version: 4 (Stream: AqZHVQR4-Pn85sUkra3, Stream Version: 10)"
          , sqlErrorDetail = ""
          , sqlErrorHint = ""
          }

  -}

  let seemsLikeTheRightErrorMessages =
        "Wrong expected version:" `Char8.isPrefixOf` sqlErrorMsg

      isTheCorrectErrorState =
        sqlState == "P0001"

      isTheCorrectExecStatus =
        sqlExecStatus == Postgres.FatalError

      isProbablyTheRightError =
        seemsLikeTheRightErrorMessages
          && isTheCorrectErrorState
          && isTheCorrectExecStatus
   in if isProbablyTheRightError
        then Just $ ExpectedVersionViolation sqlError
        else Nothing


data BatchSize
  = FixedSize NumberOfMessages
  | Unlimited
  deriving (Show, Eq)


batchSizeToInteger :: BatchSize -> Integer
batchSizeToInteger batchSize =
  case batchSize of
    FixedSize size -> toInteger size
    Unlimited -> -1


createdAtField :: RowParser Message.CreatedAt
createdAtField =
  Message.CreatedAt . Time.localTimeToUTC Time.utc <$> field


streamPositionField :: RowParser Message.StreamPosition
streamPositionField = do
  fieldWith $ \f mdata -> do
    integer <- FromField.fromField f mdata
    if integer >= 0
      then pure . Message.StreamPosition $ fromInteger integer
      else FromField.returnError FromField.Incompatible f "Stream position is negative"


fromTable :: RowParser Message
fromTable = do
  messageId <- fmap Message.MessageId field
  messageStream <- fmap StreamName field
  messageType <- fmap Message.MessageType field
  messageStreamPosition <- streamPositionField
  messageGlobalPosition <- fmap Message.GlobalPosition field
  messagePayload <- maybe Message.nullPayload Message.Payload <$> field
  messageMetadata <- maybe Message.nullMetadata Message.Metadata <$> field
  messageCreatedAt <- createdAtField

  pure Message{..}


fromFunction :: RowParser Message
fromFunction = do
  messageId <- fieldWith $ \f mdata -> do
    text <- FromField.fromField f mdata
    case UUID.fromText text of
      Nothing -> FromField.returnError FromField.Incompatible f "Invalid UUID"
      Just uuid -> pure $ Message.MessageId uuid

  messageStream <- fmap StreamName field
  messageType <- fmap Message.MessageType field
  messageStreamPosition <- streamPositionField
  messageGlobalPosition <- fmap Message.GlobalPosition field

  messagePayload <- do
    maybeByteString <- field
    pure $
      maybe
        Message.nullPayload
        Message.Payload
        (Aeson.decodeStrict =<< maybeByteString)

  messageMetadata <- do
    maybeByteString <- field
    pure $
      maybe
        Message.nullMetadata
        Message.Metadata
        (Aeson.decodeStrict =<< maybeByteString)

  messageCreatedAt <- createdAtField

  pure Message{..}


lookupById :: Postgres.Connection -> Message.MessageId -> IO (Maybe Message)
lookupById connection messageId = do
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
          FROM message_store.messages
          WHERE id = ?;
        |]

  messages <- Postgres.queryWith fromTable connection query (Postgres.Only $ Message.messageIdToUUID messageId)

  pure $ listToMaybe messages


lookupByPosition :: Postgres.Connection -> Message.GlobalPosition -> IO (Maybe Message)
lookupByPosition connection position = do
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
          FROM message_store.messages
          WHERE global_position = ?;
        |]

  messages <- Postgres.queryWith fromTable connection query (Postgres.Only $ Message.globalPositionToInteger position)

  pure $ listToMaybe messages


writeMessageWithId
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     )
  => Postgres.Connection
  -> Message.MessageId
  -> StreamName
  -> Message.MessageType
  -> payload
  -> Maybe metadata
  -> Maybe ExpectedVersion
  -> IO Message.StreamPosition
writeMessageWithId connection messageId streamName messageType payload metadata expectedVersion = do
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
        ( UUID.toText $ Message.messageIdToUUID messageId
        , streamNameToText streamName
        , Message.messageTypeToText messageType
        , Aeson.toJSON payload
        , fmap Aeson.toJSON metadata
        , fmap versionToInteger expectedVersion
        )

      handleSqlError sqlError =
        case parseExpectedVersionViolation sqlError of
          Nothing ->
            throwIO sqlError
          Just expectedVersionViolation ->
            throwIO expectedVersionViolation

  [Postgres.Only position] <-
    handle handleSqlError $
      Postgres.query connection query params

  pure (fromInteger position)


writeMessageWithId_
  :: ( Aeson.ToJSON payload
     )
  => Postgres.Connection
  -> Message.MessageId
  -> StreamName
  -> Message.MessageType
  -> payload
  -> Maybe ExpectedVersion
  -> IO Message.StreamPosition
writeMessageWithId_ connection messageId streamName messageType payload expectedVersion = do
  writeMessageWithId
    connection
    messageId
    streamName
    messageType
    payload
    (Nothing :: Maybe Message.Metadata)
    expectedVersion


-- | Write a JSON-formatted message to a named stream, optionally specifying JSON-formatted metadata and an expected version number.
writeMessage
  :: ( Aeson.ToJSON payload
     , Aeson.ToJSON metadata
     )
  => Postgres.Connection
  -> StreamName
  -> Message.MessageType
  -> payload
  -> Maybe metadata
  -> Maybe ExpectedVersion
  -> IO (Message.MessageId, Message.StreamPosition)
writeMessage connection streamName messageType payload metadata expectedVersion = do
  messageId <- Message.newMessageId
  position <- writeMessageWithId connection messageId streamName messageType payload metadata expectedVersion
  pure (messageId, position)


-- | Write a JSON-formatted message to a named stream, and optionally an expected version number.
writeMessage_
  :: ( Aeson.ToJSON payload
     )
  => Postgres.Connection
  -> StreamName
  -> Message.MessageType
  -> payload
  -> Maybe ExpectedVersion
  -> IO (Message.MessageId, Message.StreamPosition)
writeMessage_ connection streamName messageType payload expectedVersion = do
  writeMessage
    connection
    streamName
    messageType
    payload
    (Nothing :: Maybe Message.Metadata)
    expectedVersion


-- | Retrieve messages from a single stream, optionally specifying the starting position, the number of messages to retrieve, and an additional condition that will be appended to the SQL command's WHERE clause.
getStreamMessages
  :: Postgres.Connection
  -> StreamName
  -> Maybe Message.StreamPosition
  -> Maybe BatchSize
  -> Maybe Condition
  -> IO [Message]
getStreamMessages connection streamName position batchSize condition =
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
        ( streamNameToText streamName
        , maybe 0 toInteger position
        , maybe 1000 batchSizeToInteger batchSize
        , fmap conditionToText condition
        )
   in Postgres.queryWith fromFunction connection query params


-- | Retrieve messages from a category of streams, optionally specifying the starting position, the number of messages to retrieve, the correlation category for Pub/Sub, consumer group parameters, and an additional condition that will be appended to the SQL command's WHERE clause.
getCategoryMessages
  :: Postgres.Connection
  -> Category
  -> Maybe Message.GlobalPosition
  -> Maybe BatchSize
  -> Maybe Correlation
  -> Maybe ConsumerGroup
  -> Maybe Condition
  -> IO [Message]
getCategoryMessages connection category position batchSize correlation consumerGroup condition =
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
        ( categoryToText category
        , maybe 0 Message.globalPositionToInteger position
        , maybe 1000 batchSizeToInteger batchSize
        , fmap correlationToText correlation
        , fmap (toInteger . consumerGroupMember) consumerGroup
        , fmap (toInteger . consumerGroupSize) consumerGroup
        , fmap conditionToText condition
        )
   in Postgres.queryWith fromFunction connection query params


-- | Row from the messages table that corresponds to the highest position number in the stream.
getLastStreamMessage :: Postgres.Connection -> StreamName -> IO (Maybe Message)
getLastStreamMessage connection streamName =
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
        Postgres.Only (streamNameToText streamName)
   in listToMaybe <$> Postgres.queryWith fromFunction connection query params


-- | Highest position number in the stream.
streamVersion :: Postgres.Connection -> StreamName -> IO (Maybe Message.StreamPosition)
streamVersion connection streamName = do
  let query =
        [sql|
          SELECT message_store.stream_version (
            stream_name => ?
          );
        |]
      params =
        Postgres.Only (streamNameToText streamName)

  result <- Postgres.query connection query params

  pure $ case result of
    [Postgres.Only (Just position)] -> Just $ fromInteger position
    _ -> Nothing
