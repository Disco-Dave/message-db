module MessageDb.Consumer.Condition
  ( Condition (..)
  , conditionFromText
  )
where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)


newtype Condition = Condition
  { conditionToText :: Text
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


conditionFromText :: Text -> Condition
conditionFromText =
  Condition
