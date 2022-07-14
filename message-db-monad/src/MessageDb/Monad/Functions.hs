module MessageDb.Functions.Monad
  ( lookupById,
    lookupByPosition,
  )
where

import Control.Monad.Reader
import MessageDb.Extra.Monad
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message


lookupById :: (HasMessageDbData r, MonadReader r m, MonadIO m) => Message.MessageId -> m (Maybe Message)
lookupById messageId =
  withConnection $ \connection ->
    Functions.lookupById connection messageId


lookupByPosition :: (HasMessageDbData r, MonadReader r m, MonadIO m) => Message.GlobalPosition -> m (Maybe Message)
lookupByPosition position =
  withConnection $ \connection ->
    Functions.lookupByPosition connection position
