-- | The message type that is written to failure streams when using the 'writeToCategory' 'FailureStrategy'.
module MessageDb.Subscription.FailedMessage
  ( FailedMessage (..)
  , FailureReason (..)
  , handleFailures
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (liftEither)
import Data.Aeson (KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import GHC.Generics (Generic)
import MessageDb.Handlers (HandleError, Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message


-- | Reason why the message handle failed.
data FailureReason
  = HandleFailure HandleError
  | UnknownFailure Text
  deriving (Show, Eq, Generic)


instance Exception FailureReason
instance Aeson.ToJSON FailureReason
instance Aeson.FromJSON FailureReason


-- | A message that was unable to be handled.
data FailedMessage = FailedMessage
  { failedMessage :: Message
  , failedReason :: FailureReason
  }
  deriving (Show, Eq)


instance Message.HasMessageType FailedMessage


toKeyValues :: Aeson.KeyValue keyValue => FailedMessage -> [keyValue]
toKeyValues FailedMessage{..} =
  [ "message" .= failedMessage
  , "reason" .= failedReason
  ]


instance Aeson.ToJSON FailedMessage where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance Aeson.FromJSON FailedMessage where
  parseJSON = Aeson.withObject "FailedMessage" $ \object -> do
    failedMessage <- object .: "message"
    failedReason <- object .: "reason"
    pure $ FailedMessage{..}


-- | If you have a stream of 'FailedMessage' messages, then you can use
-- this function so you can handle the original messages that failed.
handleFailures :: Handlers output -> Handlers output
handleFailures originalHandlers =
  let failedMessageHandle = do
        Message.ParsedMessage{parsedPayload} <- Handlers.getParsedMessage @FailedMessage @Message.Metadata
        let originalMessage = failedMessage parsedPayload
         in liftEither $ Handlers.handle originalHandlers originalMessage
   in Handlers.addHandler (Message.messageTypeOf @FailedMessage) failedMessageHandle Handlers.emptyHandlers
