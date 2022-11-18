module MessageDb.Consumer.StreamVersion
  ( StreamVersion (..)
  )
where

import qualified Data.Aeson as Aeson
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import MessageDb.Message.StreamPosition (StreamPosition (..))


data StreamVersion
  = DoesNotExist
  | DoesExist StreamPosition
  deriving (Show, Eq, Ord)


streamVersionToInteger :: StreamVersion -> Integer
streamVersionToInteger streamVersion =
  case streamVersion of
    DoesExist version -> toInteger version
    DoesNotExist -> -1


streamVersionFromInteger :: Integer -> StreamVersion
streamVersionFromInteger integer
  | integer <= -1 = DoesNotExist
  | otherwise = DoesExist . StreamPosition $ fromInteger integer


instance Aeson.ToJSON StreamVersion where
  toJSON = Aeson.toJSON . streamVersionToInteger
  toEncoding = Aeson.toEncoding . streamVersionToInteger


instance Aeson.FromJSON StreamVersion where
  parseJSON = fmap streamVersionFromInteger . Aeson.parseJSON


instance ToField StreamVersion where
  toField = toField . streamVersionToInteger


instance FromField StreamVersion where
  fromField field =
    fmap streamVersionFromInteger . fromField field
