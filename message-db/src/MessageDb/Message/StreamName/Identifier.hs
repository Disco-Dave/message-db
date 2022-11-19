module MessageDb.Message.StreamName.Identifier
  ( Identifier (..)
  , identifierFromText
  )
where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)


-- | The identifier part of a stream name. Anything after the first hyphen (-).
newtype Identifier = Identifier
  { identifierToText :: Text
  }
  deriving
    ( Eq
    , Ord
    , IsString
    , Semigroup
    , Monoid
    , Read
    , Aeson.ToJSON
    , Aeson.FromJSON
    , FromField
    , ToField
    )
  deriving (Show) via Text


identifierFromText :: Text -> Identifier
identifierFromText =
  Identifier
