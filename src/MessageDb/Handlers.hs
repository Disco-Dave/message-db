module MessageDb.Handlers
  ( HandleError (..),
    Handler,
    projectionHandler,
    subscriptionHandler,
    Handlers,
    emptyHandlers,
    listToHandlers,
    addHandler,
    addProjectionHandler,
    addSubscriptionHandler,
    removeHandler,
    handle,
  )
where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message


-- | An error that may occur from handling a message.
data HandleError
  = HandlerParseFailure Message.ParseMessageFailure
  | HandlerNotFound
  deriving (Show, Eq)


instance Exception HandleError


type Handler state output = Message -> state -> Either HandleError output


projectionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> state -> state) ->
  Handler state state
projectionHandler original message state = do
  Message.ParsedMessage{..} <- first HandlerParseFailure $ Message.parseMessage message
  pure $ original message parsedPayload parsedMetadata state


subscriptionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> IO ()) ->
  Handler () (IO ())
subscriptionHandler original message _ = do
  Message.ParsedMessage{..} <- first HandlerParseFailure $ Message.parseMessage message
  pure $ original message parsedPayload parsedMetadata


type Handlers state output = Map Message.MessageType (Handler state output)


emptyHandlers :: Handlers status output
emptyHandlers =
  Map.empty


listToHandlers :: [(Message.MessageType, Handler state output)] -> Handlers state output
listToHandlers =
  Map.fromList


addHandler :: Message.MessageType -> Handler state output -> Handlers state output -> Handlers state output
addHandler =
  Map.insert


addProjectionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Message.MessageType ->
  (Message -> payload -> metadata -> state -> state) ->
  Handlers state state ->
  Handlers state state
addProjectionHandler messageType handler =
  addHandler messageType (projectionHandler handler)


addSubscriptionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Message.MessageType ->
  (Message -> payload -> metadata -> IO ()) ->
  Handlers () (IO ()) ->
  Handlers () (IO ())
addSubscriptionHandler messageType handler =
  addHandler messageType (subscriptionHandler handler)


removeHandler :: Message.MessageType -> Handlers status output -> Handlers status output
removeHandler =
  Map.delete


handle :: Handlers state output -> Message -> state -> Either HandleError output
handle handlers message state =
  case Map.lookup (Message.messageType message) handlers of
    Nothing ->
      Left HandlerNotFound
    Just untypedHandler ->
      untypedHandler message state
