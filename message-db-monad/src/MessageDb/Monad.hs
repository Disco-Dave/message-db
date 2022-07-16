module MessageDb.Monad
  ( MessageDbData (..)
  , defaultData
  , MonadMessageDb (..)
  , MessageDbT (..)
  , runMessageDbT
  , MessageDbIO
  , runMessageDbIO
  , withConnection
  )
where

import Control.Monad.Catch (MonadCatch, MonadThrow)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Control.Monad.Trans (MonadTrans)
import Data.Pool (Pool, withResource)
import Database.PostgreSQL.Simple (Connection)
import MessageDb.Functions (ConsumerGroup)
import MessageDb.Units (Microseconds, NumberOfMessages)
import UnliftIO (MonadUnliftIO)


data MessageDbData = MessageDbData
  { connectionPool :: Pool Connection
  , batchSize :: NumberOfMessages
  , pollInterval :: Microseconds
  , consumerGroup :: Maybe ConsumerGroup
  }


defaultData :: Pool Connection -> MessageDbData
defaultData connectionPool =
  MessageDbData
    { connectionPool = connectionPool
    , batchSize = 100
    , pollInterval = 100_000
    , consumerGroup = Nothing
    }


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


class Monad m => MonadMessageDb m where
  getMessageDbData :: m MessageDbData


instance Monad m => MonadMessageDb (MessageDbT m) where
  getMessageDbData =
    MessageDbT ask


type MessageDbIO = MessageDbT IO


runMessageDbIO :: MessageDbData -> MessageDbIO a -> IO a
runMessageDbIO =
  runMessageDbT


withConnection :: (MonadMessageDb m, MonadIO m) => (Connection -> IO a) -> m a
withConnection useConnection = do
  MessageDbData{connectionPool} <- getMessageDbData
  liftIO $ withResource connectionPool useConnection
