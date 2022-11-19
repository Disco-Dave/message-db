module MessageDb.Correlation
  ( Correlation (..)
  , correlationFromText
  )
where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)


newtype Correlation = Correlation
  { correlationToText :: Text
  }
  deriving
    ( Eq
    , Ord
    , IsString
    , Semigroup
    , Read
    , Aeson.ToJSON
    , Aeson.FromJSON
    , FromField
    , ToField
    )
  deriving (Show) via Text


correlationFromText :: Text -> Correlation
correlationFromText =
  Correlation
