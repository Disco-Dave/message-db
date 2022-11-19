module MessageDb.Message.StreamName
  ( StreamName (..)
  , streamNameFromText
  , streamCategory
  , streamIdentifier
  , addIdentifier
  , addMaybeIdentifier
  )
where

import qualified Data.Aeson as Aeson
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (ToField)
import MessageDb.Message.StreamName.Category (Category, categoryFromText, categoryToText)
import MessageDb.Message.StreamName.Identifier (Identifier (..))


-- | Name of a stream.
newtype StreamName = StreamName
  { streamNameToText :: Text
  -- ^ Convert the 'StreamName' to 'Text'.
  }
  deriving
    ( Eq
    , Ord
    , IsString
    , Semigroup
    , Monoid
    , Read
    , Aeson.ToJSON
    , Aeson.FromJSON
    , FromField
    , ToField
    )
  deriving (Show) via Text


-- | Convert a 'Text to a 'StreamName'.
streamNameFromText :: Text -> StreamName
streamNameFromText =
  StreamName


-- | Gets the category of a stream.
-- For example for "account-123" it would return "account".
streamCategory :: StreamName -> Category
streamCategory (StreamName text) =
  categoryFromText text


-- | Gets the identifier of a stream from a 'StreamName'.
-- For example "account-ed3b4af7-b4a0-499e-8a16-a09763811274" would return Just "ed3b4af7-b4a0-499e-8a16-a09763811274",
-- and "account" would return Nothing.
streamIdentifier :: StreamName -> Maybe Identifier
streamIdentifier (StreamName text) =
  let value = Text.intercalate "-" . drop 1 $ Text.split (== '-') text
   in if Text.null value
        then Nothing
        else Just $ Identifier value


-- | Add an identifier to a 'Category'.
-- For example category "account" and identifier "123" would return "account-123".
addIdentifier :: Category -> Identifier -> StreamName
addIdentifier category identifierName =
  StreamName . mconcat $
    [ categoryToText category
    , "-"
    , identifierToText identifierName
    ]


-- | Add a maybe identifier, allowing you to add an identifier to the stream name if it is Just.
addMaybeIdentifier :: Category -> Maybe Identifier -> StreamName
addMaybeIdentifier category =
  maybe (StreamName $ categoryToText category) (addIdentifier category)
