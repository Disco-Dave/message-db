module MessageDb.Monad
  ( MessageDbData (..),
    HasMessageDbData (..),
    MessageDbT (..),
    runMessageDbT,
    MessageDb,
    runMessageDb,
    withConnection,
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, ReaderT (runReaderT), asks)
import Control.Monad.Trans (MonadTrans)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import MessageDb.Units (Microseconds, NumberOfMessages)
import UnliftIO (MonadUnliftIO)


data MessageDbData = MessageDbData
  { connectionPool :: Pool Connection
  , batchSize :: NumberOfMessages
  , pollInterval :: Microseconds
  }


class HasMessageDbData r where
  getMessageDbData :: r -> MessageDbData


instance HasMessageDbData MessageDbData where
  getMessageDbData = id


newtype MessageDbT m a = MessageDbT (ReaderT MessageDbData m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadTrans
    , MonadIO
    , MonadUnliftIO
    , MonadFail
    , MonadCatch
    , MonadThrow
    , MonadReader MessageDbData
    )


runMessageDbT :: MessageDbData -> MessageDbT m a -> m a
runMessageDbT messageDbData (MessageDbT messageDbT) =
  runReaderT messageDbT messageDbData


type MessageDb = MessageDbT IO


runMessageDb :: MessageDbData -> MessageDb a -> IO a
runMessageDb =
  runMessageDbT


withConnection :: (MonadReader r m, HasMessageDbData r, MonadIO m) => (Connection -> IO a) -> m a
withConnection useConnection = do
  connections <- asks (connectionPool . getMessageDbData)
  liftIO $ withResource connections useConnection
