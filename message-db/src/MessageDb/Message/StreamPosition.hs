module MessageDb.Message.StreamPosition
  ( StreamPosition (..)
  , streamPositionFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | Position within a stream. This starts at 0 and has no gaps.
newtype StreamPosition = StreamPosition
  { streamPositionToNatural :: Natural
  -- ^ Convert a 'StreamPosition' to a 'Natural'.
  }
  deriving
    ( Show
    , Eq
    , Ord
    , Num
    , Real
    , Enum
    , Integral
    , Aeson.ToJSON
    , Aeson.FromJSON
    )


-- | Convert a 'Natural' to a 'StreamPosition'.
streamPositionFromNatural :: Natural -> StreamPosition
streamPositionFromNatural =
  StreamPosition


instance ToField StreamPosition where
  toField = toField . toInteger . streamPositionToNatural


instance FromField StreamPosition where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . streamPositionFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "Stream position is negative"
