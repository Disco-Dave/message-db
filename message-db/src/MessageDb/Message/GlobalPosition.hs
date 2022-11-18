module MessageDb.Message.GlobalPosition
  ( GlobalPosition (..)
  , globalPositionFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | Primary key. The ordinal position of the message in the entire message store. Global position may have gaps.
newtype GlobalPosition = GlobalPosition
  { globalPositionToNatural :: Natural
  -- ^ Convert a 'GlobalPosition' to a 'Natural'.
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


-- | Convert a 'Natural' to a 'GlobalPosition'.
globalPositionFromNatural :: Natural -> GlobalPosition
globalPositionFromNatural =
  GlobalPosition


instance ToField GlobalPosition where
  toField = toField . toInteger . globalPositionToNatural


instance FromField GlobalPosition where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . globalPositionFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "Global position is negative"
