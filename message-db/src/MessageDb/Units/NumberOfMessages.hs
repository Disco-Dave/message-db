-- | Different units of measures used throughout the library that I wasn't able to find a better home for.
module MessageDb.Units.NumberOfMessages
  ( NumberOfMessages (..)
  , numberOfMessageFromNatural
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Numeric.Natural (Natural)


-- | A number of messages. Must be 0 or greater.
newtype NumberOfMessages = NumberOfMessages
  { numberOfMessagesToNatural :: Natural
  -- ^ Convert 'NumberOfMessages' to a 'Natural'.
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


numberOfMessageFromNatural :: Natural -> NumberOfMessages
numberOfMessageFromNatural =
  NumberOfMessages


instance ToField NumberOfMessages where
  toField = toField . toInteger . numberOfMessagesToNatural


instance FromField NumberOfMessages where
  fromField field metadata = do
    integer <- fromField field metadata
    if integer >= 0
      then pure . numberOfMessageFromNatural $ fromInteger integer
      else FromField.returnError FromField.Incompatible field "Number of messages is negative"
