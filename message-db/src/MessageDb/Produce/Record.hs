module MessageDb.Produce.Record
  ( ProduceRecord (..)
  , produceRecord
  )
where

import MessageDb.Message.MessageType (HasMessageType (getMessageType), MessageType)
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


produceRecord
  :: forall payload
   . HasMessageType payload
  => StreamName
  -> payload
  -> ProduceRecord payload Metadata
produceRecord streamName payload =
  ProduceRecord
    { produceStreamName = streamName
    , produceMessageType = getMessageType @payload
    , producePayload = payload
    , produceMetadata = Nothing
    , produceExpectedVersion = Nothing
    }
