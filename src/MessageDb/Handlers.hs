-- | Maps functions to handle different message types.
module MessageDb.Handlers
  ( Handler,
    TypedHandler,
    Handlers,
    HandleError (..),
    empty,
    attach,
    detach,
    handle,
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON)
import Data.Bifunctor (first)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Message (Message, MessageType)
import qualified MessageDb.Message as Message
import MessageDb.TypedMessage (ConversionFailure, TypedMessage, typed)


-- | An error that may occur from handling a message.
data HandleError
  = MessageConversionFailure ConversionFailure
  | MessageHandlerNotFound
  deriving (Show, Eq)


instance Exception HandleError


-- | An untyped handler of a message.
type Handler state output = Message -> state -> Either HandleError output


{- | A typed handler of a message.
 If you don't care about the type of payload then use 'Message.Payload'.
 If you don't care about the type of metadata then use 'Message.Metadata'.
-}
type TypedHandler state output payload metadata = TypedMessage payload metadata -> state -> output


-- | A map of handlers for different messae types.
type Handlers state output = Map MessageType (Handler state output)


-- | A set of handlers.
empty :: Handlers state output
empty =
  Map.empty


-- | Attach a function to handle different message types.
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


-- | Detach a function for a message type.
detach :: MessageType -> Handlers state output -> Handlers state output
detach =
  Map.delete


-- | Convert the handlers to a function that handles different messages.
handle :: Handlers state output -> Handler state output
handle handlers message state =
  case Map.lookup (Message.messageType message) handlers of
    Nothing ->
      Left MessageHandlerNotFound
    Just untypedHandler ->
      untypedHandler message state
