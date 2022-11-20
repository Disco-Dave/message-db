module MessageDb.Consumer.ProjectionHandlers
  ( ProjectionHandler
  , ProjectionHandlers
  , emptyProjectionHandlers
  , addProjectionHandler
  , addProjectionHandler_
  , addProjectionHandlerAny
  , addProjectionHandlerAny_
  , handleProjection
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Consumer.HandlerError (HandlerError (..))
import MessageDb.Consumer.HandlerKey (HandlerKey (..), lookupByMessageType)
import MessageDb.Message (Message, ParseMessageError, UntypedMessage)
import qualified MessageDb.Message as Message
import MessageDb.Message.MessageType (MessageType)
import MessageDb.Message.Metadata (Metadata)


type ProjectionHandler state =
  UntypedMessage -> state -> Either ParseMessageError state


type ProjectionHandlers state =
  Map HandlerKey (ProjectionHandler state)


emptyProjectionHandlers :: ProjectionHandlers state
emptyProjectionHandlers =
  Map.empty


addProjectionHandler
  :: ( Aeson.FromJSON payload
     , Aeson.FromJSON metadata
     )
  => MessageType
  -> (Message payload metadata -> state -> state)
  -> ProjectionHandlers state
  -> ProjectionHandlers state
addProjectionHandler messageType handler handlers =
  let untypedHandler untypedMessage currentState =
        Message.parseMessage untypedMessage <&> \message ->
          handler message currentState
   in Map.insert (ByMessageType messageType) untypedHandler handlers


addProjectionHandler_
  :: ( Aeson.FromJSON payload
     )
  => MessageType
  -> (Message payload Metadata -> state -> state)
  -> ProjectionHandlers state
  -> ProjectionHandlers state
addProjectionHandler_ =
  addProjectionHandler


addProjectionHandlerAny
  :: ( Aeson.FromJSON payload
     , Aeson.FromJSON metadata
     )
  => (Message payload metadata -> state -> state)
  -> ProjectionHandlers state
  -> ProjectionHandlers state
addProjectionHandlerAny handler handlers =
  let untypedHandler untypedMessage currentState =
        Message.parseMessage untypedMessage <&> \message ->
          handler message currentState
   in Map.insert AnyMessageType untypedHandler handlers


addProjectionHandlerAny_
  :: ( Aeson.FromJSON payload
     )
  => (Message payload Metadata -> state -> state)
  -> ProjectionHandlers state
  -> ProjectionHandlers state
addProjectionHandlerAny_ =
  addProjectionHandlerAny


handleProjection
  :: ProjectionHandlers state
  -> UntypedMessage
  -> state
  -> Either HandlerError state
handleProjection handlers message state =
  case lookupByMessageType (Message.messageType message) handlers of
    Nothing -> Left HandlerNotFound
    Just handler ->
      first HandlerParseError $
        handler message state
