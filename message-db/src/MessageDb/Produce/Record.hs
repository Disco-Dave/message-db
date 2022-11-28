module MessageDb.Produce.Record
  ( ProduceRecord (..)
  , produceRecord
  )
where

import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.Metadata (Metadata)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.StreamVersion (StreamVersion)


data ProduceRecord payload metadata = ProduceRecord
  { produceStreamName :: StreamName
  , produceMessageType :: MessageType
  , producePayload :: payload
  , produceMetadata :: Maybe metadata
  , produceExpectedVersion :: Maybe StreamVersion
  }
  deriving (Show, Eq)


produceRecord :: StreamName -> MessageType -> payload -> ProduceRecord payload Metadata
produceRecord streamName messageType payload =
  ProduceRecord
    { produceStreamName = streamName
    , produceMessageType = messageType
    , producePayload = payload
    , produceMetadata = Nothing
    , produceExpectedVersion = Nothing
    }
