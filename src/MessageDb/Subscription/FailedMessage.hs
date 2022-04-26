module MessageDb.Subscription.FailedMessage
  ( FailedMessage (..),
    messageType,
    handleFailures,
  )
where

import Data.Aeson (KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import MessageDb.Handlers (Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import qualified MessageDb.TypedMessage as TypedMessage


data FailedMessage = FailedMessage
  { message :: Message
  , reason :: Text
  }


messageType :: Message.MessageType
messageType =
  "FailedMessage"


toKeyValues :: Aeson.KeyValue keyValue => FailedMessage -> [keyValue]
toKeyValues FailedMessage{..} =
  [ "message" .= message
  , "reason" .= reason
  ]


instance Aeson.ToJSON FailedMessage where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance Aeson.FromJSON FailedMessage where
  parseJSON = Aeson.withObject "FailedMessage" $ \object -> do
    message <- object .: "message"
    reason <- object .: "reason"
    pure $ FailedMessage{..}


handleFailures :: Handlers state output -> Handlers state output
handleFailures originalHandlers =
  let failedMessageHandle untypedMessage state =
        case Message.typedPayload @FailedMessage untypedMessage of
          Left payloadReason ->
            Left . Handlers.MessageConversionFailure $
              TypedMessage.ConversionFailure
                { failedPayloadReason = Just payloadReason
                , failedMetadataReason = Nothing
                }
          Right failedMessage ->
            let originalMessage = message failedMessage
                originalType = Message.messageType originalMessage
             in Handlers.handle originalType originalHandlers originalMessage state
   in Map.insert messageType failedMessageHandle Handlers.empty
