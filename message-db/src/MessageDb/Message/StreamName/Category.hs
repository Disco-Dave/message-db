module MessageDb.Message.StreamName.Category
  ( Category
  , categoryToText
  , categoryFromText
  )
where

import qualified Data.Aeson as Aeson
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField)


-- | A 'Category' represents everything in the 'StreamName' up to the first hyphen (-).
-- For example, the category for the stream name, "account-1234", is "account".
newtype Category = Category Text
  deriving
    ( Eq
    , Ord
    , IsString
    , Semigroup
    , Read
    , Aeson.ToJSON
    , ToField
    )
  deriving (Show) via Text


categoryToText :: Category -> Text
categoryToText =
  coerce


categoryFromText :: Text -> Category
categoryFromText text =
  case Text.split (== '-') text of
    (name : _) -> Category name
    _ -> Category "" -- 'Text.split' never returns an empty list


instance Aeson.FromJSON Category where
  parseJSON = fmap categoryFromText . Aeson.parseJSON


instance FromField Category where
  fromField field metadata = do
    categoryFromText <$> fromField field metadata
