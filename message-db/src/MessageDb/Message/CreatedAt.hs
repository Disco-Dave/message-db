module MessageDb.Message.CreatedAt
  ( CreatedAt (..)
  , createdAtFromUTCTime
  )
where

import Control.Applicative (Alternative ((<|>)))
import qualified Data.Aeson as Aeson
import Data.Time (UTCTime)
import qualified Data.Time as Time
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField)


-- | Timestamp when the message was written.
newtype CreatedAt = CreatedAt
  { createdAtToUTCTime :: UTCTime
  -- ^ Convert a 'GlobalPosition' to a 'Natural'.
  }
  deriving
    ( Eq
    , Ord
    , Aeson.ToJSON
    , Aeson.FromJSON
    , ToField
    )
  deriving (Show) via UTCTime


-- | Convert a 'UTCTime' to a 'CreatedAt.
createdAtFromUTCTime :: UTCTime -> CreatedAt
createdAtFromUTCTime =
  CreatedAt


instance FromField CreatedAt where
  fromField field metadata =
    let fromTimestamp = do
          localTime <- fromField field metadata

          pure . createdAtFromUTCTime $
            Time.localTimeToUTC Time.utc localTime

        fromTimestampz =
          createdAtFromUTCTime <$> fromField field metadata
     in fromTimestamp <|> fromTimestampz
