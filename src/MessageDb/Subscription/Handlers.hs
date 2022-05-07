module MessageDb.Subscription.Handlers
  ( SubscriptionHandlers,
    empty,
    attachMessage,
    attach,
    detach,
    handle,
  )
where

import qualified Data.Aeson as Aeson
import MessageDb.Handlers (HandleError, Handlers, NoState)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType, Metadata)
import MessageDb.TypedMessage (TypedMessage (TypedMessage))
import qualified MessageDb.TypedMessage as TypedMessage


type SubscriptionHandlers = Handlers NoState (IO ())


empty :: SubscriptionHandlers
empty =
  Handlers.empty


attachMessage :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> IO ()) -> SubscriptionHandlers -> SubscriptionHandlers
attachMessage messageType handler =
  Handlers.attachMessage messageType $ \typedMessage _ ->
    handler typedMessage


attach :: Aeson.FromJSON payload => MessageType -> (payload -> IO ()) -> SubscriptionHandlers -> SubscriptionHandlers
attach messageType handler =
  attachMessage @_ @(Maybe Metadata) messageType $ \TypedMessage{payload} ->
    handler payload


detach :: MessageType -> SubscriptionHandlers -> SubscriptionHandlers
detach =
  Handlers.detach


handle :: MessageType -> SubscriptionHandlers -> Message -> Either HandleError (IO ())
handle messageType handlers message =
  Handlers.handle messageType handlers message ()
