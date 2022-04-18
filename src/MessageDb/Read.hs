module MessageDb.Read where

import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.TypedMessage (TypedMessage)
import qualified MessageDb.TypedMessage as TypedMessage

data Projection entity = Projection
  { identity :: entity
  , handlers :: Handlers entity entity
  }

project :: Projection entity -> [Message] -> Either Handlers.Error entity
project Projection{..} messages =
  undefined
