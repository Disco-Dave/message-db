module MessageDb.Consumer.HandlerError
  ( HandlerError (..)
  )
where

import Control.Exception (Exception)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import MessageDb.Message (ParseMessageError)


data HandlerError
  = HandlerNotFound
  | HandlerParseError ParseMessageError
  deriving (Show, Eq, Generic)


instance Exception HandlerError
instance Aeson.ToJSON HandlerError
instance Aeson.FromJSON HandlerError
