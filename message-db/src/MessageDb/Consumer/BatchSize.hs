module MessageDb.Consumer.BatchSize
  ( BatchSize (..)
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import MessageDb.Units.NumberOfMessages (NumberOfMessages (..))


data BatchSize
  = FixedSize NumberOfMessages
  | Unlimited
  deriving (Show, Eq)


batchSizeToInteger :: BatchSize -> Integer
batchSizeToInteger batchSize =
  case batchSize of
    FixedSize size -> toInteger size
    Unlimited -> -1


batchSizeFromInteger :: Integer -> BatchSize
batchSizeFromInteger integer
  | integer <= -1 = Unlimited
  | otherwise = FixedSize . NumberOfMessages $ fromInteger integer


instance Aeson.ToJSON BatchSize where
  toJSON = Aeson.toJSON . batchSizeToInteger
  toEncoding = Aeson.toEncoding . batchSizeToInteger


instance Aeson.FromJSON BatchSize where
  parseJSON = fmap batchSizeFromInteger . Aeson.parseJSON


instance ToField BatchSize where
  toField = toField . batchSizeToInteger


instance FromField BatchSize where
  fromField field =
    fmap batchSizeFromInteger . fromField field
