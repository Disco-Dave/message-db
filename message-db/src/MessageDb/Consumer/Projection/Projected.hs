module MessageDb.Consumer.Projection.Projected
  ( Projected (..)
  , initProjected
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.Projection.Error (ProjectionError (..))
import MessageDb.Message.MessageType (HasMessageType, MessageTypeIs)
import MessageDb.StreamVersion (StreamVersion)
import qualified MessageDb.StreamVersion as StreamVersion


-- | A projected state
data Projected state = Projected
  { projectedErrors :: [ProjectionError]
  , projectedStreamVersion :: StreamVersion
  , projectedState :: state
  }
  deriving (Show, Eq, Functor)
  deriving (HasMessageType) via (MessageTypeIs "MessageDb.Projected" (Projected state))


initProjected :: state -> Projected state
initProjected initialState =
  Projected
    { projectedErrors = []
    , projectedStreamVersion = StreamVersion.DoesNotExist
    , projectedState = initialState
    }


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
