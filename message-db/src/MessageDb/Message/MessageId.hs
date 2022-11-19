module MessageDb.Message.MessageId
  ( MessageId (..)
  , newMessageId
  , messageIdFromUUID
  )
where

import Control.Applicative (Alternative ((<|>)))
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import Database.PostgreSQL.Simple.FromField (FromField (..))
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)


-- | Unique id of a message. Most be unique across the entire event store.
newtype MessageId = MessageId
  { messageIdToUUID :: UUID
  -- ^ Convert a 'MessageId' to a 'UUID'.
  }
  deriving
    ( Eq
    , Ord
    , Aeson.FromJSON
    , Aeson.ToJSON
    , ToField
    )
  deriving (Show) via UUID


-- | Create a new unique message id.
newMessageId :: MonadIO m => m MessageId
newMessageId =
  liftIO $ fmap MessageId UUID.V4.nextRandom


-- | Convert a 'UUID to a 'MessageId.
messageIdFromUUID :: UUID -> MessageId
messageIdFromUUID =
  MessageId


-- | The 'MessageId' is stored as a @uuid@ in the @message_store.messages@ table.
-- However, the 'MessageId' is cast to a @text@ when using the @message-db@ stored functions.
-- This instance tries both.
instance FromField MessageId where
  fromField field metadata =
    let fromUuid =
          messageIdFromUUID <$> fromField field metadata

        fromText = do
          text <- fromField field metadata
          case UUID.fromText text of
            Nothing -> FromField.returnError FromField.Incompatible field "Invalid UUID"
            Just uuid -> pure $ messageIdFromUUID uuid
     in fromUuid <|> fromText
