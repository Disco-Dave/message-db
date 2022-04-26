module MessageDb.Subscription.Handlers
  ( SubscriptionHandlers
  , empty
  , attach
  , detach
  , handle
  )
where

import qualified Data.Aeson as Aeson
import MessageDb.Handlers (Handlers, NoState, HandleError)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import MessageDb.TypedMessage (TypedMessage)


type SubscriptionHandlers = Handlers NoState (IO ())


empty :: SubscriptionHandlers
empty =
  Handlers.empty


attach :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> IO ()) -> SubscriptionHandlers -> SubscriptionHandlers
attach messageType handler =
  Handlers.attach messageType $ \typedMessage _ ->
    handler typedMessage


detach :: MessageType -> SubscriptionHandlers -> SubscriptionHandlers
detach =
  Handlers.detach


handle :: MessageType -> SubscriptionHandlers -> Message -> Either HandleError (IO ())
handle messageType handlers message =
  Handlers.handle messageType handlers message ()
