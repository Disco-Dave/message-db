-- | The message type that is written to failure streams when using the 'writeToCategory' 'FailureStrategy'.
module MessageDb.Subscription.FailedMessage
  ( FailedMessage (..),
    messageType,
    handleFailures,
  )
where

import Control.Monad.Except (liftEither)
import Data.Aeson (KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import MessageDb.Handlers (Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message


-- | A message that was unable to be handled.
data FailedMessage = FailedMessage
  { message :: Message
  , reason :: Text
  }


-- | The message type of a 'FailedMessage'.
messageType :: Message.MessageType
messageType =
  Message.messageTypeOf @FailedMessage


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
handleFailures :: Handlers output -> Handlers output
handleFailures originalHandlers =
  let failedMessageHandle = do
        Message.ParsedMessage{parsedPayload} <- Handlers.getParsedMessage @FailedMessage @Message.Metadata
        let originalMessage = message parsedPayload
         in liftEither $ Handlers.handle originalHandlers originalMessage
   in Handlers.addHandler messageType failedMessageHandle Handlers.emptyHandlers
