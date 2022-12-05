module MessageDb.Consumer.Subscription.Error
  ( SubscriptionError (..)
  )
where

import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.Subscription.ErrorReason (SubscriptionErrorReason)
import MessageDb.Message (UntypedMessage)
import MessageDb.Message.MessageType (HasMessageType, MessageTypeIs (..))


data SubscriptionError = SubscriptionError
  { subscriptionErrorReason :: SubscriptionErrorReason
  , subscriptionErrorMessage :: UntypedMessage
  }
  deriving (Show)
  deriving (HasMessageType) via (MessageTypeIs "MessageDb.SubscriptionError" SubscriptionError)


instance Exception SubscriptionError


toKeyValues :: Aeson.KeyValue kv => SubscriptionError -> [kv]
toKeyValues SubscriptionError{..} =
  [ "reason" .= subscriptionErrorReason
  , "message" .= subscriptionErrorMessage
  ]


instance Aeson.ToJSON SubscriptionError where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues
