module MessageDb.Consumer.ConsumerIndex
  ( ConsumerIndex (..)
  , consumerIndexFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | Time in consumerIndex. Must be 0 or greater.
newtype ConsumerIndex = ConsumerIndex
  { consumerIndexToNatural :: Natural
  -- ^ Convert 'ConsumerIndex' to a 'Natural'.
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


consumerIndexFromNatural :: Natural -> ConsumerIndex
consumerIndexFromNatural =
  ConsumerIndex


instance ToField ConsumerIndex where
  toField = toField . toInteger . consumerIndexToNatural


instance FromField ConsumerIndex where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . consumerIndexFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "ConsumerIndex is negative"
