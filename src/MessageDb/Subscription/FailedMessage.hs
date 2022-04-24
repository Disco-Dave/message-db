module MessageDb.Subscription.FailedMessage (
  FailedMessage (..),
  failMessage,
) where

import Control.Exception.Safe (SomeException)
import Data.Aeson (KeyValue ((.=)), (.:))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as Text
import MessageDb.Message (Message)

data FailedMessage = FailedMessage
  { message :: Message
  , exception :: Text
  }

failMessage :: Message -> SomeException -> FailedMessage
failMessage message exception =
  FailedMessage
    { message = message
    , exception = Text.pack $ show exception
    }

toKeyValues :: Aeson.KeyValue keyValue => FailedMessage -> [keyValue]
toKeyValues FailedMessage{..} =
  [ "message" .= message
  , "exception" .= exception
  ]

instance Aeson.ToJSON FailedMessage where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues

instance Aeson.FromJSON FailedMessage where
  parseJSON = Aeson.withObject "FailedMessage" $ \object -> do
    message <- object .: "message"
    exception <- object .: "exception"
    pure $ FailedMessage{..}
