module MessageDb.Message.Payload
  ( Payload (..)
  , nullPayload
  , payloadFromValue
  , toPayload
  , fromPayload
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


-- | JSON representation of the message body.
newtype Payload = Payload
  { payloadToValue :: Aeson.Value
  -- ^ Convert a 'Payload' to a 'Aeson.Value'.
  }
  deriving
    ( Eq
    , Aeson.ToJSON
    , Aeson.FromJSON
    , ToField
    )


instance Show Payload where
  show (Payload value) =
    Char8.unpack $ Aeson.encode value


-- | Convert a 'Aeson.Value' to a 'Payload'.
payloadFromValue :: Aeson.Value -> Payload
payloadFromValue =
  Payload


-- | Empty 'Payload'.
nullPayload :: Payload
nullPayload =
  Payload Aeson.Null


-- | Render any type to a JSON 'Payload'.
toPayload :: Aeson.ToJSON payload => payload -> Payload
toPayload =
  Payload . Aeson.toJSON


-- | Parse the JSON 'Payload'.
fromPayload :: Aeson.FromJSON payload => Payload -> Either Text payload
fromPayload (Payload payload) =
  first Text.pack $ AesonTypes.parseEither Aeson.parseJSON payload


-- | The 'Payload' is stored as a @jsonb@ in the @message_store.messages@ table.
-- However, the 'Payload' is cast to a @varchar@ when using the @message-db@ stored functions.
-- This instance tries both.
instance FromField Payload where
  fromField field metadata =
    let fromJsonb =
          maybe nullPayload payloadFromValue
            <$> fromField field metadata

        fromVarchar = do
          bytes <- fromMaybe "null" <$> fromField field metadata

          case Aeson.eitherDecodeStrict bytes of
            Right value ->
              pure $ payloadFromValue value
            Left err ->
              FromField.returnError FromField.Incompatible field err
     in fromVarchar <|> fromJsonb
