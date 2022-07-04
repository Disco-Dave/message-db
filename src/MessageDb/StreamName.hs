{- | Streams are the fundamental unit of organization of evented, service-oriented systems.
 They are both the storage and the transport of messages in message-based systems.
 And they are the principle storage medium of applicative entity data.

 Streams are created by writing a message to the stream. Messages are appended to the end of streams.
 If the stream doesn't exist when an event is appended to it, the event will be appended at position 0.
 If the stream already exists, the event will be appended at the next position number.

 Read more at: http://docs.eventide-project.org/core-concepts/streams
-}
module MessageDb.StreamName
  ( StreamName (..)
  , Category
  , categoryOfStream
  , categoryToText
  , category
  , Identifier (..)
  , identifierOfStream
  , addIdentifierToCategory
  , addMaybeIdentifierToCategory
  )
where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text


-- | Name of a stream. Look into 'categoryOfStream' and 'identifierOfStream' to parse out the category or identifier in the stream name.
newtype StreamName = StreamName
  { streamNameToText :: Text
  }
  deriving (Eq, Ord, IsString, Semigroup)
  deriving (Show) via Text


instance Aeson.ToJSON StreamName where
  toJSON = Aeson.toJSON . streamNameToText
  toEncoding = Aeson.toEncoding . streamNameToText


instance Aeson.FromJSON StreamName where
  parseJSON = fmap StreamName . Aeson.parseJSON


separator :: Char
separator = '-'


{- | A category stream name does not have an ID.
 For example, the stream name for the category of all accounts is "account".
-}
newtype Category = Category Text
  deriving (Eq, Ord)
  deriving (Show) via Text


-- | Converts from a 'Category' to a normal 'Text'.
categoryToText :: Category -> Text
categoryToText (Category text) =
  text


{- | Gets the category of a stream.
 For example for "account-123" it would return "account".
-}
categoryOfStream :: StreamName -> Category
categoryOfStream (StreamName text) =
  case Text.split (== separator) text of
    (name : _) -> Category name
    _ -> Category "" -- 'Text.split' never returns an empty list


category :: Text -> Category
category =
  categoryOfStream . StreamName


instance Aeson.ToJSON Category where
  toJSON = Aeson.toJSON . categoryToText
  toEncoding = Aeson.toEncoding . categoryToText


instance Aeson.FromJSON Category where
  parseJSON = fmap Category . Aeson.parseJSON


-- | The identifier part of a stream name. Anything after the first hypen (-).
newtype Identifier = Identifier
  { identifierNameToText :: Text
  }
  deriving (Eq, Ord)
  deriving (Show) via Text


{- | Gets the identifier of a stream from a 'StreamName'.
 For example "account-ed3b4af7-b4a0-499e-8a16-a09763811274" would return Just "ed3b4af7-b4a0-499e-8a16-a09763811274",
 and "account" would return Nothing.
-}
identifierOfStream :: StreamName -> Maybe Identifier
identifierOfStream (StreamName text) =
  let separatorText = Text.pack [separator]
      value = Text.intercalate separatorText . drop 1 $ Text.split (== separator) text
   in if Text.null value
        then Nothing
        else Just $ Identifier value


{- | Add an identifier to a 'Category'.
 For example category "account" and identifier "123" would return "account-123".
-}
addIdentifierToCategory :: Category -> Identifier -> StreamName
addIdentifierToCategory (Category categoryText) identifierName =
  StreamName $ categoryText <> Text.singleton separator <> identifierNameToText identifierName


-- | Add a maybe identifier, allowing you to add an identifier to the stream name if it is Just.
addMaybeIdentifierToCategory :: Category -> Maybe Identifier -> StreamName
addMaybeIdentifierToCategory categoryText maybeIdentifier =
  case maybeIdentifier of
    Nothing ->
      coerce categoryText
    Just identifierName ->
      addIdentifierToCategory categoryText identifierName


instance Aeson.ToJSON Identifier where
  toJSON = Aeson.toJSON . identifierNameToText
  toEncoding = Aeson.toEncoding . identifierNameToText


instance Aeson.FromJSON Identifier where
  parseJSON = fmap Identifier . Aeson.parseJSON
