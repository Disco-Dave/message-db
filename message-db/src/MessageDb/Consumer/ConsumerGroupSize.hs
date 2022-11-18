module MessageDb.Consumer.ConsumerGroupSize
  ( ConsumerGroupSize (..)
  , consumerGroupSizeFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | Time in consumerGroupSize. Must be 0 or greater.
newtype ConsumerGroupSize = ConsumerGroupSize
  { consumerGroupSizeToNatural :: Natural
  -- ^ Convert 'ConsumerGroupSize' to a 'Natural'.
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


consumerGroupSizeFromNatural :: Natural -> ConsumerGroupSize
consumerGroupSizeFromNatural =
  ConsumerGroupSize


instance ToField ConsumerGroupSize where
  toField = toField . toInteger . consumerGroupSizeToNatural


instance FromField ConsumerGroupSize where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . consumerGroupSizeFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "ConsumerGroupSize is negative"

