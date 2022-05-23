-- | Assigns message handlers per message type for performing subscriptions.
module MessageDb.Subscription.Handlers
  ( SubscriptionHandlers,
    empty,
    attach,
    detach,
    handle,
  )
where

import qualified Data.Aeson as Aeson
import MessageDb.Handlers (HandleError, Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import MessageDb.TypedMessage (TypedMessage)


-- | Map functions to message types for Subscriptions.
type SubscriptionHandlers = Handlers () (IO ())


-- | An empty map of subscription handlers.
empty :: SubscriptionHandlers
empty =
  Handlers.empty


-- | Attach a function to handle a message type for a subscription.
attach ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  MessageType ->
  (TypedMessage payload metadata -> IO ()) ->
  SubscriptionHandlers ->
  SubscriptionHandlers
attach messageType handler =
  Handlers.attach messageType $ \typedMessage _ ->
    handler typedMessage


-- | Detach a function to hadle a message type for a subscription.
detach :: MessageType -> SubscriptionHandlers -> SubscriptionHandlers
detach =
  Handlers.detach


-- | Handle a message type for a subscription.
handle :: SubscriptionHandlers -> Message -> Either HandleError (IO ())
handle handlers message =
  Handlers.handle handlers message ()
