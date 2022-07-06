{-# LANGUAGE QuasiQuotes #-}

module MessageDb.Temp
  ( withDatabaseUrl
  , withConnection
  )
where

import Control.Exception (Exception, IOException)
import Control.Exception.Safe (bracket, throwIO)
import Control.Monad (void)
import Control.Monad.Catch (Handler (Handler))
import qualified Control.Retry as Retry
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (getLast)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified Database.PostgreSQL.Simple as Simple
import Database.PostgreSQL.Simple.Options (Options)
import qualified Database.PostgreSQL.Simple.Options as PostgresOptions
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.Postgres.Temp (Accum (Merge))
import qualified Database.Postgres.Temp as PostgresTemp
import qualified Paths_message_db_temp
import System.Environment (getEnvironment)
import qualified System.Process.Typed as Process


migrate :: Options -> IO ()
migrate options = do
  hostEnv <- getEnvironment

  installScriptPath <-
    Paths_message_db_temp.getDataFileName "official-message-db-upstream/database/install.sh"

  let fromLast =
        fromMaybe "" . getLast

      processEnv =
        hostEnv
          <> [
               ( "PGHOST"
               , fromLast $ PostgresOptions.host options
               )
             ,
               ( "PGDATABASE"
               , fromLast $ PostgresOptions.dbname options
               )
             ,
               ( "PGPORT"
               , fromLast . fmap show $ PostgresOptions.port options
               )
             ,
               ( "PGUSER"
               , fromLast $ PostgresOptions.user options
               )
             ]
      command = Process.proc installScriptPath []
   in void . Process.readProcess_ $ Process.setEnv processEnv command

  let url = PostgresOptions.toConnectionString options
   in bracket (Simple.connectPostgreSQL url) Simple.close $ \connection -> do
        let query =
              [sql|
                CREATE ROLE test_user 
                WITH LOGIN PASSWORD 'password' 
                IN ROLE message_store;
              |]
         in void $ Simple.execute_ connection query

        let query =
              [sql|
                ALTER ROLE test_user 
                SET search_path TO message_store,public;
              |]
         in void $ Simple.execute_ connection query


withDatabaseUrl :: (ByteString -> IO a) -> IO a
withDatabaseUrl use = do
  let tempConfig =
        mempty
          { PostgresTemp.connectionOptions =
              mempty
                { PostgresOptions.user = pure "postgres"
                }
          , PostgresTemp.initDbConfig =
              Merge $
                mempty
                  { PostgresTemp.commandLine =
                      mempty{PostgresTemp.keyBased = Map.fromList [("--username=", Just "postgres")]}
                  }
          , PostgresTemp.postgresConfigFile =
              [ ("message_store.sql_condition", "on")
              ]
          }

  let retryPolicy =
        Retry.limitRetriesByCumulativeDelay 32_000 $
          Retry.exponentialBackoff 1_000

      exceptionHandlers =
        let restartFor :: forall e a. Exception e => a -> Handler IO Bool
            restartFor _ = Handler @_ @_ @e $ \_ -> pure True
         in [ restartFor @PostgresTemp.StartError
            , restartFor @IOException
            ]

  Retry.recovering retryPolicy exceptionHandlers $ \_ -> do
    result <- PostgresTemp.withConfig tempConfig $ \db ->
      let options = PostgresTemp.toConnectionOptions db
          dbUrl =
            PostgresOptions.toConnectionString $
              options
                { PostgresOptions.user = pure "test_user"
                , PostgresOptions.password = pure "password"
                , PostgresOptions.dbname = pure "message_store"
                }
       in migrate options *> use dbUrl

    case result of
      Left err -> throwIO err
      Right value -> pure value


withConnection :: (Postgres.Connection -> IO a) -> IO a
withConnection use =
  withDatabaseUrl $ \url ->
    bracket (Simple.connectPostgreSQL url) Simple.close use
