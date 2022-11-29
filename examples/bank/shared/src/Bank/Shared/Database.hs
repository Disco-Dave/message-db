module Bank.Shared.Database
  ( DatabaseConfig (..)
  , withConnectionPool
  , withConnection
  )
where

import Control.Exception.Annotated.UnliftIO (checkpointCallStack)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader, asks)
import Data.Has (Has (..))
import Data.Pool (Pool)
import Data.Pool qualified as Pool
import Database.PostgreSQL.Simple qualified as Postgres
import Bank.Shared.Database.ConnectionUrl (ConnectionUrl)
import Bank.Shared.Database.ConnectionUrl qualified as ConnectionUrl
import Bank.Shared.Database.IdleConnectionTimeout (IdleConnectionTimeout)
import Bank.Shared.Database.IdleConnectionTimeout qualified as IdleConnectionTimeout
import Bank.Shared.Database.MaxConnections (MaxConnections)
import Bank.Shared.Database.MaxConnections qualified as MaxConnections
import GHC.Stack (HasCallStack)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket)


data DatabaseConfig = DatabaseConfig
  { connectionUrl :: ConnectionUrl
  , idleConnectionTimeout :: IdleConnectionTimeout
  , maxConnections :: MaxConnections
  }


withConnectionPool :: (MonadUnliftIO m, HasCallStack) => DatabaseConfig -> (Pool Postgres.Connection -> m a) -> m a
withConnectionPool config use =
  checkpointCallStack $
    let createPool =
          liftIO . Pool.newPool $
            Pool.PoolConfig
              { Pool.createResource = ConnectionUrl.open config.connectionUrl
              , Pool.freeResource = ConnectionUrl.close
              , Pool.poolCacheTTL = IdleConnectionTimeout.toDouble config.idleConnectionTimeout
              , Pool.poolMaxResources = MaxConnections.toInt config.maxConnections
              }
        destroyPool = liftIO . Pool.destroyAllResources
     in bracket createPool destroyPool use


withConnection
  :: ( MonadUnliftIO m
     , MonadReader r m
     , Has (Pool Postgres.Connection) r
     , HasCallStack
     )
  => (Postgres.Connection -> IO a)
  -> m a
withConnection useConnection =
  checkpointCallStack $ do
    connectionPool <- asks getter
    liftIO $ Pool.withResource connectionPool useConnection
