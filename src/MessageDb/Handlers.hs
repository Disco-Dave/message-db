module MessageDb.Handlers
  ( HandleError (..),
    Handler,
    ProjectionHandler,
    SubscriptionHandler,
    projectionHandler,
    subscriptionHandler,
    Handlers,
    ProjectionHandlers,
    SubscriptionHandlers,
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


type Handler' output = Message -> output


type ProjectionHandler' state = Handler' (state -> state)


type SubscriptionHandler' = Handler' (IO ())


type Handler state output = Message -> state -> Either HandleError output


type ProjectionHandler state = Handler state state


type SubscriptionHandler = Handler () (IO ())


projectionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> state -> state) ->
  ProjectionHandler state
projectionHandler original message state = do
  Message.ParsedMessage{..} <- first HandlerParseFailure $ Message.parseMessage message
  pure $ original message parsedPayload parsedMetadata state


subscriptionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> IO ()) ->
  SubscriptionHandler
subscriptionHandler original message _ = do
  Message.ParsedMessage{..} <- first HandlerParseFailure $ Message.parseMessage message
  pure $ original message parsedPayload parsedMetadata


type Handlers state output = Map Message.MessageType (Handler state output)


type ProjectionHandlers state = Handlers state state


type SubscriptionHandlers = Handlers () (IO ())


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
  ProjectionHandlers state ->
  ProjectionHandlers state
addProjectionHandler messageType handler =
  addHandler messageType (projectionHandler handler)


addSubscriptionHandler ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Message.MessageType ->
  (Message -> payload -> metadata -> IO ()) ->
  SubscriptionHandlers ->
  SubscriptionHandlers
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
