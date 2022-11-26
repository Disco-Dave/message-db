module MessageDb.Consumer.ProjectionError
  ( ProjectionError (..)
  )
where

import Control.Exception (Exception)
import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.HandlerError (HandlerError)
import MessageDb.Message (UntypedMessage)


data ProjectionError = ProjectionError
  { projectionErrorReason :: HandlerError
  , projectionErrorMessage :: UntypedMessage
  }
  deriving (Show, Eq)


instance Exception ProjectionError


toKeyValues :: Aeson.KeyValue kv => ProjectionError -> [kv]
toKeyValues ProjectionError{..} =
  [ "reason" .= projectionErrorReason
  , "message" .= projectionErrorMessage
  ]


instance Aeson.ToJSON ProjectionError where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues
