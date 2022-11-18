module MessageDb.Units.Microseconds
  ( Microseconds (..)
  , microsecondsFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | Time in microseconds. Must be 0 or greater.
newtype Microseconds = Microseconds
  { microsecondsToNatural :: Natural
  -- ^ Convert 'Microseconds' to a 'Natural'.
  }
  deriving
    ( Eq
    , Ord
    , Num
    , Real
    , Enum
    , Integral
    , Aeson.ToJSON
    , Aeson.FromJSON
    )
  deriving (Show) via Natural


microsecondsFromNatural :: Natural -> Microseconds
microsecondsFromNatural =
  Microseconds


instance ToField Microseconds where
  toField = toField . toInteger . microsecondsToNatural


instance FromField Microseconds where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . microsecondsFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "Microseconds is negative"
