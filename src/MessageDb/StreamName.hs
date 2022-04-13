module MessageDb.StreamName (
  StreamName (..),
  CategoryName,
  category,
  fromCategoryName,
  IdentityName,
  identity,
  fromIdentityName,
  all,
) where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Simple.ToField as ToField
import Prelude hiding (all)

newtype StreamName = StreamName
  { fromStreamName :: Text
  }
  deriving (Show, Eq, Ord, IsString)

instance Aeson.ToJSON StreamName where
  toJSON = Aeson.toJSON . fromStreamName
  toEncoding = Aeson.toEncoding . fromStreamName

instance Aeson.FromJSON StreamName where
  parseJSON = fmap StreamName . Aeson.parseJSON

instance ToField StreamName where
  toField = ToField.toField . fromStreamName

instance FromField StreamName where
  fromField = fmap (fmap StreamName) . FromField.fromField

separator :: Char
separator = '-'

newtype CategoryName = CategoryName Text
  deriving (Show, Eq, Ord)

fromCategoryName :: CategoryName -> Text
fromCategoryName (CategoryName text) =
  text

all :: CategoryName
all =
  CategoryName ""

category :: StreamName -> CategoryName
category (StreamName text) =
  case Text.split (== separator) text of
    (name : _) -> CategoryName name
    _ -> all -- 'Text.split' never returns an empty list

instance Aeson.ToJSON CategoryName where
  toJSON = Aeson.toJSON . fromCategoryName
  toEncoding = Aeson.toEncoding . fromCategoryName

instance Aeson.FromJSON CategoryName where
  parseJSON = fmap CategoryName . Aeson.parseJSON

instance ToField CategoryName where
  toField = ToField.toField . fromCategoryName

instance FromField CategoryName where
  fromField = fmap (fmap CategoryName) . FromField.fromField

newtype IdentityName = IdentityName Text
  deriving (Show, Eq, Ord)

fromIdentityName :: IdentityName -> Text
fromIdentityName (IdentityName text) =
  text

identity :: StreamName -> Maybe IdentityName
identity (StreamName text) =
  let separatorText = Text.pack [separator]
      value = Text.intercalate separatorText . drop 1 $ Text.split (== separator) text
   in if Text.null value
        then Nothing
        else Just $ IdentityName value

instance Aeson.ToJSON IdentityName where
  toJSON = Aeson.toJSON . fromIdentityName
  toEncoding = Aeson.toEncoding . fromIdentityName

instance Aeson.FromJSON IdentityName where
  parseJSON = fmap IdentityName . Aeson.parseJSON

instance ToField IdentityName where
  toField = ToField.toField . fromIdentityName

instance FromField IdentityName where
  fromField = fmap (fmap IdentityName) . FromField.fromField
