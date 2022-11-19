module MessageDb.Producer.ProduceRecord
  ( ProduceRecord (..)
  , emptyProduceRecord
  )
where

import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.Metadata (Metadata)
import MessageDb.Message.Payload (Payload, nullPayload)
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


type UntypedProduceRecord = ProduceRecord Payload Metadata


emptyProduceRecord :: UntypedProduceRecord
emptyProduceRecord =
  ProduceRecord
    { produceStreamName = mempty
    , produceMessageType = mempty
    , producePayload = nullPayload
    , produceMetadata = Nothing
    , produceExpectedVersion = Nothing
    }
