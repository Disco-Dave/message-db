{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module MessageDb.Handlers2 where

--  ( HandleError (..),
--    Handler,
--    Handlers,
--    State,
--    Output,
--    MessageHandler (..),
--    emptyHandlers,
--    listToHandlers,
--    addHandler,
--    removeHandler,
--    handle,
--  )

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Kind (Type)
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


type Handler state output = Message -> state -> Either Message.ParseMessageFailure output


type Handlers state output = Map Message.MessageType (Handler state output)


class MessageHandler handler state output | handler -> state output where
  toHandler :: handler -> Handler state output


instance MessageHandler (Message -> IO ()) () (IO ()) where
  toHandler original message _ =
    pure $ original message


instance Aeson.FromJSON payload => MessageHandler (Message -> payload -> IO ()) () (IO ()) where
  toHandler original message _ = do
    Message.ParsedMessage{..} <- Message.parseMessage @payload @Message.Metadata message
    pure $ original message parsedPayload


instance (Aeson.FromJSON payload, Aeson.FromJSON metadata) => MessageHandler (Message -> payload -> metadata -> IO ()) () (IO ()) where
  toHandler original message _ = do
    Message.ParsedMessage{..} <- Message.parseMessage @payload @metadata message
    pure $ original message parsedPayload parsedMetadata


instance MessageHandler (Handler state output) state output where
  toHandler = id


instance (Aeson.FromJSON payload, Aeson.FromJSON metadata) => MessageHandler (Message -> payload -> metadata -> state -> output) state output where
  toHandler original message state = do
    Message.ParsedMessage{..} <- Message.parseMessage @payload @metadata message
    pure $ original message parsedPayload parsedMetadata state

instance ( Aeson.FromJSON payload) => MessageHandler (Message -> payload -> state -> output) state output where
  toHandler original message state = do
    Message.ParsedMessage{..} <- Message.parseMessage @payload @Message.Metadata message
    pure $ original message parsedPayload state

{-

instance
  ( State (Message -> state -> output) ~ state
  , Output (Message -> state -> output) ~ output
  ) =>
  MessageHandler (Message -> state -> output)
  where
  toHandler original message state = do
    pure $ original message state

emptyHandlers :: Handlers status output
emptyHandlers =
  Map.empty

listToHandlers :: [(Message.MessageType, Handler state output)] -> Handlers state output
listToHandlers =
  Map.fromList

addHandler :: MessageHandler handler => Message.MessageType -> handler -> Handlers (State handler) (Output handler) -> Handlers (State handler) (Output handler)
addHandler messageType handler =
  Map.insert messageType (toHandler handler)

removeHandler :: Message.MessageType -> Handlers status output -> Handlers status output
removeHandler =
  Map.delete

handle :: Handlers state output -> Message -> state -> Either HandleError output
handle handlers message state =
  case Map.lookup (Message.messageType message) handlers of
    Nothing ->
      Left HandlerNotFound
    Just untypedHandler ->
      first HandlerParseFailure $ untypedHandler message state

-}
