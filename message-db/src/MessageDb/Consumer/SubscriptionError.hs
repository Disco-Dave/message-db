module MessageDb.Consumer.SubscriptionError
  ( SubscriptionError (..)
  )
where

import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.SubscriptionErrorReason (SubscriptionErrorReason)
import MessageDb.Message (UntypedMessage)


data SubscriptionError = SubscriptionError
  { subscriptionErrorReason :: SubscriptionErrorReason
  , subscriptionErrorMessage :: UntypedMessage
  }
  deriving (Show)


instance Exception SubscriptionError


toKeyValues :: Aeson.KeyValue kv => SubscriptionError -> [kv]
toKeyValues SubscriptionError{..} =
  [ "reason" .= subscriptionErrorReason
  , "message" .= subscriptionErrorMessage
  ]


instance Aeson.ToJSON SubscriptionError where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues
