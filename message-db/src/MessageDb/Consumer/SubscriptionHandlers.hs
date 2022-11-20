module MessageDb.Consumer.SubscriptionHandlers
  ( SubscriptionHandler
  , SubscriptionHandlers
  , emptySubscriptionHandlers
  , addSubscriptionHandler
  , addSubscriptionHandler_
  , addSubscriptionHandlerAny
  , addSubscriptionHandlerAny_
  , handleSubscription
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Consumer.HandlerError (HandlerError (..))
import MessageDb.Consumer.HandlerKey (HandlerKey (..), lookupByMessageType)
import MessageDb.Message (Message, ParseMessageError, UntypedMessage)
import qualified MessageDb.Message as Message
import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.Metadata (Metadata)


type SubscriptionHandler m =
  UntypedMessage -> Either ParseMessageError (m ())


type SubscriptionHandlers m =
  Map HandlerKey (SubscriptionHandler m)


emptySubscriptionHandlers :: SubscriptionHandlers m
emptySubscriptionHandlers =
  Map.empty


addSubscriptionHandler
  :: ( Aeson.FromJSON payload
     , Aeson.FromJSON metadata
     )
  => MessageType
  -> (Message payload metadata -> m ())
  -> SubscriptionHandlers m
  -> SubscriptionHandlers m
addSubscriptionHandler messageType handler handlers =
  let untypedHandler untypedMessage =
        handler <$> Message.parseMessage untypedMessage
   in Map.insert (ByMessageType messageType) untypedHandler handlers


addSubscriptionHandler_
  :: ( Aeson.FromJSON payload
     )
  => MessageType
  -> (Message payload Metadata -> m ())
  -> SubscriptionHandlers m
  -> SubscriptionHandlers m
addSubscriptionHandler_ =
  addSubscriptionHandler


addSubscriptionHandlerAny
  :: ( Aeson.FromJSON payload
     , Aeson.FromJSON metadata
     )
  => (Message payload metadata -> m ())
  -> SubscriptionHandlers m
  -> SubscriptionHandlers m
addSubscriptionHandlerAny handler handlers =
  let untypedHandler untypedMessage =
        handler <$> Message.parseMessage untypedMessage
   in Map.insert AnyMessageType untypedHandler handlers


addSubscriptionHandlerAny_
  :: ( Aeson.FromJSON payload
     )
  => (Message payload Metadata -> m ())
  -> SubscriptionHandlers m
  -> SubscriptionHandlers m
addSubscriptionHandlerAny_ =
  addSubscriptionHandlerAny


handleSubscription
  :: SubscriptionHandlers m
  -> UntypedMessage
  -> Either HandlerError (m ())
handleSubscription handlers message =
  case lookupByMessageType (Message.messageType message) handlers of
    Nothing -> Left HandlerNotFound
    Just handler ->
      first HandlerParseError $
        handler message
