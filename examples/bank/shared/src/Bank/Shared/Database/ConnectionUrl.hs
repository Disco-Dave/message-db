-- | PostgreSQL URL Format:
-- postgresql://[user[:password]@][netloc][:port][/dbname][?param1=value1&...]
--
-- Parameters for query string portion: https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-PARAMKEYWORDS
module Bank.Shared.Database.ConnectionUrl
  ( ConnectionUrl
  , fromByteString
  , toByteString
  , open
  , close
  , withConnection
  )
where

import Control.Exception.Annotated.UnliftIO (checkpointCallStack)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.Coerce (coerce)
import Database.PostgreSQL.Simple qualified as Postgres
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)


-- | Connection URL to a PostgreSQL database.
newtype ConnectionUrl = ConnectionUrl ByteString


-- | Convert a strict 'ByteString' to a 'ConnectionUrl' if:
-- 1. Not empty or all whitespace
--
-- /Note/ Leading and trailing white space will be stripped off before parsing.
fromByteString :: ByteString -> Maybe ConnectionUrl
fromByteString byteString =
  let strippedByteString = Char8.strip byteString
   in if Char8.null strippedByteString
        then Nothing
        else Just $ ConnectionUrl strippedByteString


-- | Downgrade the 'ConnectionUrl' to an unvalidated 'ByteString'.
toByteString :: ConnectionUrl -> ByteString
toByteString =
  coerce


open :: (MonadUnliftIO m, HasCallStack) => ConnectionUrl -> m Postgres.Connection
open =
  checkpointCallStack . liftIO . Postgres.connectPostgreSQL . coerce


close :: (MonadUnliftIO m, HasCallStack) => Postgres.Connection -> m ()
close =
  checkpointCallStack . liftIO . Postgres.close . coerce


withConnection :: (MonadUnliftIO m, HasCallStack) => ConnectionUrl -> (Postgres.Connection -> m a) -> m a
withConnection url =
  checkpointCallStack . bracket (open url) close
