module MessageDb.Consumer.Projection
  ( Projection (..)
  , Projected (..)
  , emptyProjection
  , project
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import Data.Foldable (Foldable (foldl', toList))
import Data.List.NonEmpty (NonEmpty (..))
import MessageDb.Consumer.ProjectionError (ProjectionError (..))
import MessageDb.Consumer.ProjectionHandlers (ProjectionHandlers, handleProjection)
import MessageDb.Message (UntypedMessage)
import qualified MessageDb.Message as Message
import MessageDb.StreamVersion (StreamVersion)
import qualified MessageDb.StreamVersion as StreamVersion


-- | Defines how to perform a projection a stream.
data Projection state = Projection
  { projectionState :: state
  , projectionHandlers :: ProjectionHandlers state
  }


-- | A projected state
data Projected state = Projected
  { projectedErrors :: [ProjectionError]
  , projectedStreamVersion :: StreamVersion
  , projectedState :: state
  }
  deriving (Show, Eq, Functor)


projectedToKeyValues
  :: ( Aeson.KeyValue kv
     , Aeson.ToJSON state
     )
  => Projected state
  -> [kv]
projectedToKeyValues Projected{..} =
  [ "errors" .= projectedErrors
  , "streamVersion" .= projectedStreamVersion
  , "state" .= projectedState
  ]


instance Aeson.ToJSON state => Aeson.ToJSON (Projected state) where
  toJSON = Aeson.object . projectedToKeyValues
  toEncoding = Aeson.pairs . mconcat . projectedToKeyValues


instance Aeson.FromJSON state => Aeson.FromJSON (Projected state) where
  parseJSON = Aeson.withObject "Projected" $ \object -> do
    projectedErrors <- object .: "errors"
    projectedStreamVersion <- object .: "streamVersion"
    projectedState <- object .: "state"
    pure Projected{..}


-- | Constructs an empty projection.
emptyProjection :: state -> Projected state
emptyProjection projectionState =
  Projected
    { projectedErrors = []
    , projectedStreamVersion = StreamVersion.DoesNotExist
    , projectedState = projectionState
    }


-- | Project a state of a stream by aggregating messages.
project :: Projection state -> NonEmpty UntypedMessage -> Projected state
project Projection{projectionState, projectionHandlers} messages =
  let applyHandler projected@Projected{projectedState, projectedErrors} message =
        let updatedProjected =
              projected
                { projectedStreamVersion =
                    StreamVersion.DoesExist $ Message.messageStreamPosition message
                }
         in case handleProjection projectionHandlers message projectedState of
              Right updatedState ->
                updatedProjected
                  { projectedState = updatedState
                  }
              Left reason ->
                updatedProjected
                  { projectedErrors =
                      ProjectionError reason message : projectedErrors
                  }
   in foldl' applyHandler (emptyProjection projectionState) (toList messages)
