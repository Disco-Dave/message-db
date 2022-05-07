module TestApp
  ( TestAppData (..),
    withTestAppData,
    TestApp (..),
    runWith,
    run,
    withConnection,
  )
where

import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import qualified TempMessageDb
import UnliftIO (MonadUnliftIO (withRunInIO))
import UnliftIO.Exception (bracket)


newtype TestAppData = TestAppData
  { connectionPool :: Pool Postgres.Connection
  }


withTestAppData :: (TestAppData -> IO value) -> IO value
withTestAppData =
  runContT $ do
    databaseUrl <-
      ContT TempMessageDb.withDatabaseUrl

    connectionPool <-
      let createPool =
            let createConnection = Postgres.connectPostgreSQL databaseUrl
                destroyConnection = Postgres.close
             in Pool.createPool createConnection destroyConnection 1 15 2

          destroyPool =
            Pool.destroyAllResources
       in ContT $ bracket createPool destroyPool

    pure
      TestAppData
        { connectionPool = connectionPool
        }


newtype TestApp value = TestApp
  { fromTestApp :: ReaderT TestAppData IO value
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadUnliftIO
    , MonadReader TestAppData
    )


runWith :: TestAppData -> TestApp value -> IO value
runWith testAppData testApp =
  runReaderT (fromTestApp testApp) testAppData


run :: TestApp value -> IO value
run testApp =
  withTestAppData $ \testAppData ->
    runWith testAppData testApp


withConnection :: (Postgres.Connection -> TestApp value) -> TestApp value
withConnection useConnection = do
  TestAppData{connectionPool} <- ask

  withRunInIO $ \runInIO ->
    Pool.withResource connectionPool (runInIO . useConnection)
