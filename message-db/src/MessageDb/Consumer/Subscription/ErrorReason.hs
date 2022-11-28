module MessageDb.Consumer.Subscription.ErrorReason
  ( SubscriptionErrorReason (..)
  )
where

import Control.Exception (Exception, SomeException)
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.HandlerError (HandlerError)


data SubscriptionErrorReason
  = SubscriptionHandlerError HandlerError
  | SubscriptionException SomeException
  deriving (Show)


instance Exception SubscriptionErrorReason


instance Aeson.ToJSON SubscriptionErrorReason where
  toJSON = \case
    SubscriptionHandlerError err ->
      Aeson.toJSON err
    SubscriptionException exception ->
      Aeson.toJSON $ show exception
