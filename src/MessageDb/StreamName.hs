{- | Streams are the fundamental unit of organization of evented, service-oriented systems.
 They are both the storage and the transport of messages in message-based systems.
 And they are the principle storage medium of applicative entity data.

 Streams are created by writing a message to the stream. Messages are appended to the end of streams.
 If the stream doesn't exist when an event is appended to it, the event will be appended at position 0.
 If the stream already exists, the event will be appended at the next position number.

 Read more at: http://docs.eventide-project.org/core-concepts/streams
-}
module MessageDb.StreamName
  ( StreamName (..),
    CategoryName,
    category,
    fromCategoryName,
    IdentityName (..),
    identity,
    addIdentity,
    addMaybeIdentity,
  )
where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Prelude hiding (all)


-- | Name of a stream.
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


{- | A category stream name does not have an ID.
 For example, the stream name for the category of all accounts is "account".
-}
newtype CategoryName = CategoryName Text
  deriving (Eq, Ord)
  deriving (Show) via Text


-- | Converts from a 'CategoryName' to a nromal 'Text'.
fromCategoryName :: CategoryName -> Text
fromCategoryName (CategoryName text) =
  text


{- | Gets the category of a stream.
 For example for "account-123" it would return "account".
-}
category :: StreamName -> CategoryName
category (StreamName text) =
  case Text.split (== separator) text of
    (name : _) -> CategoryName name
    _ -> CategoryName "" -- 'Text.split' never returns an empty list


instance Aeson.ToJSON CategoryName where
  toJSON = Aeson.toJSON . fromCategoryName
  toEncoding = Aeson.toEncoding . fromCategoryName


instance Aeson.FromJSON CategoryName where
  parseJSON = fmap CategoryName . Aeson.parseJSON


-- | The identifier part of a stream name. Anything after the first hypen (-).
newtype IdentityName = IdentityName {fromIdentityName :: Text}
  deriving (Eq, Ord)
  deriving (Show) via Text


{- | Gets the identifier of a stream from a 'StreamName'.
 For example "account-ed3b4af7-b4a0-499e-8a16-a09763811274" would return Just "ed3b4af7-b4a0-499e-8a16-a09763811274",
 and "account" would return Nothing.
-}
identity :: StreamName -> Maybe IdentityName
identity (StreamName text) =
  let separatorText = Text.pack [separator]
      value = Text.intercalate separatorText . drop 1 $ Text.split (== separator) text
   in if Text.null value
        then Nothing
        else Just $ IdentityName value


{- | Add an identifier to a 'CategoryName'.
 For example category "account" and identity "123" would return "account-123".
-}
addIdentity :: CategoryName -> IdentityName -> StreamName
addIdentity (CategoryName categoryName) identityName =
  StreamName $ categoryName <> Text.singleton separator <> fromIdentityName identityName


-- | Add a maybe identifier, allowing you to add an identifier to the stream name if it is Just.
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
