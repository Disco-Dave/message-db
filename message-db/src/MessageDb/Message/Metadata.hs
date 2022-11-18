module MessageDb.Message.Metadata
  ( Metadata (..)
  , nullMetadata
  , metadataFromValue
  , toMetadata
  , fromMetadata
  )
where

import Control.Applicative (Alternative ((<|>)))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import Data.Bifunctor (Bifunctor (first))
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)


-- | JSON representation of the message metadata.
newtype Metadata = Metadata
  { metadataToValue :: Aeson.Value
  -- ^ Convert a 'Metadata' to a 'Aeson.Value'.
  }
  deriving
    ( Eq
    , Aeson.ToJSON
    , Aeson.FromJSON
    , ToField
    )


instance Show Metadata where
  show (Metadata value) =
    Char8.unpack $ Aeson.encode value


-- | Convert a 'Aeson.Value' to a 'Metadata'.
metadataFromValue :: Aeson.Value -> Metadata
metadataFromValue =
  Metadata


-- | Empty 'Metadata'.
nullMetadata :: Metadata
nullMetadata =
  Metadata Aeson.Null


-- | Render any type to a JSON 'Metadata'.
toMetadata :: Aeson.ToJSON metadata => metadata -> Metadata
toMetadata =
  Metadata . Aeson.toJSON


-- | Parse the JSON 'Metadata'.
fromMetadata :: Aeson.FromJSON metadata => Metadata -> Either Text metadata
fromMetadata (Metadata metadata) =
  first Text.pack $ AesonTypes.parseEither Aeson.parseJSON metadata


-- | The 'Metadata' is stored as a @jsonb@ in the @message_store.messages@ table.
-- However, the 'Metadata' is cast to a @varchar@ when using the @message-db@ stored functions.
-- This instance tries both.
instance FromField Metadata where
  fromField field metadata =
    let fromJsonb =
          maybe nullMetadata metadataFromValue
            <$> fromField field metadata

        fromVarchar = do
          bytes <- fromMaybe "null" <$> fromField field metadata

          case Aeson.eitherDecodeStrict bytes of
            Right value ->
              pure $ metadataFromValue value
            Left err ->
              FromField.returnError FromField.Incompatible field err
     in fromVarchar <|> fromJsonb
