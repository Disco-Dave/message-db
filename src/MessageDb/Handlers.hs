module MessageDb.Handlers
  ( Handler
  , TypedHandler
  , NoState
  , AnyPayload
  , AnyMetadata
  , Handlers
  , HandleError (..)
  , empty
  , attach
  , detach
  , handle
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Message (Message, MessageType, Metadata, Payload)
import MessageDb.TypedMessage (ConversionFailure, TypedMessage, typed)


data HandleError
  = MessageConversionFailure ConversionFailure
  | MessageHandlerNotFound
  deriving (Show, Eq)
instance Exception HandleError


type Handler state output = Message -> state -> Either HandleError output
type TypedHandler state output payload metadata = TypedMessage payload metadata -> state -> output


type NoState = ()
type AnyPayload = Maybe Payload
type AnyMetadata = Maybe Metadata


type Handlers state output = Map MessageType (Handler state output)


empty :: Handlers state output
empty =
  Map.empty


attach ::
  (FromJSON payload, FromJSON metadata) =>
  MessageType ->
  TypedHandler state output payload metadata ->
  Handlers state output ->
  Handlers state output
attach messageType typedHandler handlers =
  let handler message state = do
        typedMessage <- first MessageConversionFailure $ typed message
        pure $ typedHandler typedMessage state
   in Map.insert messageType handler handlers


detach :: MessageType -> Handlers state output -> Handlers state output
detach =
  Map.delete


handle :: MessageType -> Handlers state output -> Handler state output
handle messageType handlers message state =
  case Map.lookup messageType handlers of
    Nothing ->
      Left MessageHandlerNotFound
    Just untypedHandler ->
      untypedHandler message state
