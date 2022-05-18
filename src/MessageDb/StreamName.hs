module MessageDb.StreamName
  ( StreamName (..),
    CategoryName,
    category,
    fromCategoryName,
    IdentityName (..),
    identity,
    addIdentity,
    all,
    addMaybeIdentity,
  )
where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (all)


newtype StreamName = StreamName
  { fromStreamName :: Text
  }
  deriving (Eq, Ord, IsString, Semigroup)
  deriving (Show) via Text


instance Aeson.ToJSON StreamName where
  toJSON = Aeson.toJSON . fromStreamName
  toEncoding = Aeson.toEncoding . fromStreamName


instance Aeson.FromJSON StreamName where
  parseJSON = fmap StreamName . Aeson.parseJSON


separator :: Char
separator = '-'


newtype CategoryName = CategoryName Text
  deriving (Eq, Ord)
  deriving (Show) via Text


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


newtype IdentityName = IdentityName {fromIdentityName :: Text}
  deriving (Eq, Ord)
  deriving (Show) via Text


identity :: StreamName -> Maybe IdentityName
identity (StreamName text) =
  let separatorText = Text.pack [separator]
      value = Text.intercalate separatorText . drop 1 $ Text.split (== separator) text
   in if Text.null value
        then Nothing
        else Just $ IdentityName value


addIdentity :: CategoryName -> IdentityName -> StreamName
addIdentity (CategoryName categoryName) identityName =
  StreamName $ categoryName <> Text.singleton separator <> fromIdentityName identityName


addMaybeIdentity :: CategoryName -> Maybe IdentityName -> StreamName
addMaybeIdentity categoryName maybeIdentityName =
  case maybeIdentityName of
    Nothing ->
      coerce categoryName
    Just identityName ->
      addIdentity categoryName identityName


instance Aeson.ToJSON IdentityName where
  toJSON = Aeson.toJSON . fromIdentityName
  toEncoding = Aeson.toEncoding . fromIdentityName


instance Aeson.FromJSON IdentityName where
  parseJSON = fmap IdentityName . Aeson.parseJSON
