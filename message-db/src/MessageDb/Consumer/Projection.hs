module MessageDb.Consumer.Projection
  ( Projection (..)
  , initProjection
  , addProjection
  , addProjection_
  , project
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Foldable (Foldable (foldl', toList))
import Data.Functor ((<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Consumer.HandlerError (HandlerError (..))
import MessageDb.Consumer.Projection.Error (ProjectionError (..))
import MessageDb.Consumer.Projection.Projected (Projected (..), initProjected)
import MessageDb.Message (Message (..), ParseMessageError (..), UntypedMessage)
import qualified MessageDb.Message as Message
import MessageDb.Message.MessageType (HasMessageType (getMessageType), MessageType (..))
import MessageDb.Message.Metadata (Metadata (..))
import qualified MessageDb.StreamVersion as StreamVersion


-- | Defines how to perform a projection a stream.
data Projection state = Projection
  { projectionState :: state
  , projectionDefaultHandler :: Maybe (UntypedMessage -> state -> state)
  , projectionHandlers :: Map MessageType (UntypedMessage -> state -> Either ParseMessageError state)
  }


initProjection :: state -> Projection state
initProjection initialState =
  Projection
    { projectionState = initialState
    , projectionDefaultHandler = Nothing
    , projectionHandlers = Map.empty
    }


addProjection
  :: forall payload metadata state
   . ( Aeson.FromJSON payload
     , Aeson.FromJSON metadata
     , HasMessageType payload
     )
  => (Message payload metadata -> state -> state)
  -> Projection state
  -> Projection state
addProjection handler projection =
  let untypedHandler untypedMessage currentState =
        Message.parseMessage untypedMessage <&> \message ->
          handler message currentState
      oldHandlers =
        projectionHandlers projection
      updatedHandlers =
        Map.insert (getMessageType @payload) untypedHandler oldHandlers
   in projection{projectionHandlers = updatedHandlers}


addProjection_
  :: forall payload state
   . ( Aeson.FromJSON payload
     , HasMessageType payload
     )
  => (Message payload Metadata -> state -> state)
  -> Projection state
  -> Projection state
addProjection_ =
  addProjection


-- | Project a state of a stream by aggregating messages.
project :: Projection state -> NonEmpty UntypedMessage -> Projected state
project projection messages =
  let Projection
        { projectionState
        , projectionHandlers
        , projectionDefaultHandler
        } = projection

      handleProjection message currentState =
        let messageTypeHandler =
              Map.lookup (messageType message) projectionHandlers
         in case (messageTypeHandler, projectionDefaultHandler) of
              (Nothing, Just handler) ->
                Right $ handler message currentState
              (Just handler, _) ->
                first HandlerParseError $
                  handler message currentState
              _ ->
                Left HandlerNotFound

      applyHandler projected@Projected{projectedState, projectedErrors} message =
        let updatedProjected =
              projected
                { projectedStreamVersion =
                    StreamVersion.DoesExist $ Message.messageStreamPosition message
                }
         in case handleProjection message projectedState of
              Right updatedState ->
                updatedProjected
                  { projectedState = updatedState
                  }
              Left reason ->
                updatedProjected
                  { projectedErrors =
                      ProjectionError reason message : projectedErrors
                  }
   in foldl' applyHandler (initProjected projectionState) (toList messages)
