module MessageDb.Read where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message)
import MessageDb.Message.Typed (TypedMessage)
import qualified MessageDb.Message as Message
import qualified MessageDb.Message.Typed as TypedMessage

type Handler entity = Message -> entity -> Either TypedMessage.ConversionFailure entity
type Handlers entity = Map Message.MessageType (Handler entity)

addHandler :: (Aeson.FromJSON payload, Aeson.FromJSON metadata) => Message.MessageType -> (TypedMessage payload metadata -> entity -> entity) -> Handlers entity -> Handlers entity
addHandler messageType handle handlers = undefined

data Projection entity = Projection
  { identity :: entity
  , handlers :: Map Message.MessageType (Handler entity)
  }

project :: Projection entity -> [Message] -> Either TypedMessage.ConversionFailure entity
project Projection{..} messages =
  undefined
