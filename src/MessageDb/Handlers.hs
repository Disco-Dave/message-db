module MessageDb.Handlers (
  Handler,
  TypedHandler,
  NoState,
  AnyPayload,
  AnyMetadata,
  Handlers,
  HandleError (..),
  empty,
  attach,
  detach,
  handle,
) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Message (Message, MessageType, Metadata, Payload)
import MessageDb.TypedMessage (ConversionFailure, TypedMessage, typed)

data HandleError
  = MessageConversionFailure Message ConversionFailure
  | MessageHandlerNotFound Message
  deriving (Show, Eq)
instance Exception HandleError

type Handler state output = Message -> state -> Either HandleError output
type TypedHandler state output payload metadata = TypedMessage payload metadata -> state -> output

type NoState = ()
type AnyPayload = Maybe Payload
type AnyMetadata = Maybe Metadata

newtype Handlers state output = Handlers (Map MessageType (Handler state output))

empty :: Handlers state output
empty =
  Handlers Map.empty

attach ::
  (FromJSON payload, FromJSON metadata) =>
  MessageType ->
  TypedHandler state output payload metadata ->
  Handlers state output ->
  Handlers state output
attach messageType typedHandler (Handlers handlers) =
  let handler message state = do
        typedMessage <- first (MessageConversionFailure message) $ typed message
        pure $ typedHandler typedMessage state
   in Handlers $ Map.insert messageType handler handlers

detach :: MessageType -> Handlers state output -> Handlers state output
detach messageType (Handlers handlers) =
  Handlers $ Map.delete messageType handlers

handle :: MessageType -> Handlers state output -> Handler state output
handle messageType (Handlers handlers) message state =
  case Map.lookup messageType handlers of
    Nothing ->
      Left $ MessageHandlerNotFound message
    Just untypedHandler ->
      untypedHandler message state
