-- | The message type that is written to failure streams when using the 'writeToCategory' 'FailureStrategy'.
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


-- | A message that was unable to be handled.
data FailedMessage = FailedMessage
  { message :: Message
  , reason :: Text
  }


-- | The message type of a 'FailedMessage'.
messageType :: Message.MessageType
messageType =
  Message.typeOf @FailedMessage


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


{- | If you have a stream of 'FailedMessage' messages, then you can use
 this function so you can handle the original messages that failed.
-}
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
             in Handlers.handle originalHandlers originalMessage state
   in Map.insert messageType failedMessageHandle Handlers.empty
