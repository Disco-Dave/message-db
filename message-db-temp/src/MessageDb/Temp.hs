{-# LANGUAGE QuasiQuotes #-}

module MessageDb.Temp
  ( ConnectionStrings (..)
  , withConnectionStrings
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
import Database.PostgreSQL.Simple.Options (Options)
import qualified Database.PostgreSQL.Simple.Options as PostgresOptions
import Database.PostgreSQL.Simple.SqlQQ (sql)
import Database.Postgres.Temp (Accum (Merge))
import qualified Database.Postgres.Temp as PostgresTemp
import qualified Paths_message_db_temp
import System.Environment (getEnvironment)
import qualified System.Process.Typed as Process


-- | Connection strings used to connect to your temporary message-db.
data ConnectionStrings = ConnectionStrings
  { privilegedConnectionString :: ByteString
  -- ^ Connection string used to connect to the database as a privileged user.
  , normalConnectionString :: ByteString
  -- ^ Connection string used to connect to the database as a user that only has the 'message_store' role.
  }


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
      command = Process.proc "bash" [installScriptPath]
   in void . Process.readProcess_ $ Process.setEnv processEnv command

  let url = PostgresOptions.toConnectionString options
   in bracket (Postgres.connectPostgreSQL url) Postgres.close $ \connection -> do
        let query =
              [sql|
                CREATE ROLE normal_user 
                WITH LOGIN PASSWORD 'password' 
                IN ROLE message_store;
              |]
         in void $ Postgres.execute_ connection query

        let query =
              [sql|
                ALTER ROLE normal_user 
                SET search_path TO message_store,public;
              |]
         in void $ Postgres.execute_ connection query

        let query =
              [sql|
                ALTER ROLE privileged_user
                SET search_path TO message_store,public;
              |]
         in void $ Postgres.execute_ connection query


-- | Create and use a temporary message-db for testing.
withConnectionStrings :: (ConnectionStrings -> IO a) -> IO a
withConnectionStrings use = do
  let tempConfig =
        mempty
          { PostgresTemp.connectionOptions =
              mempty
                { PostgresOptions.user = pure "privileged_user"
                }
          , PostgresTemp.initDbConfig =
              Merge $
                mempty
                  { PostgresTemp.commandLine =
                      mempty{PostgresTemp.keyBased = Map.fromList [("--username=", Just "privileged_user")]}
                  }
          , PostgresTemp.postgresConfigFile =
              [ ("message_store.sql_condition", "on")
              ]
          }

  let retryPolicy =
        Retry.limitRetries 10

      exceptionHandlers =
        let restartFor :: forall e a. Exception e => a -> Handler IO Bool
            restartFor _ = Handler @_ @_ @e $ \_ -> pure True
         in [ restartFor @PostgresTemp.StartError
            , restartFor @IOException
            ]

  Retry.recovering retryPolicy exceptionHandlers $ \_ -> do
    result <- PostgresTemp.withConfig tempConfig $ \db -> do
      let options =
            PostgresTemp.toConnectionOptions db

          tempMessageDb =
            let privilegedConnectionString =
                  PostgresOptions.toConnectionString $
                    options
                      { PostgresOptions.dbname = pure "message_store"
                      }

                normalConnectionString =
                  PostgresOptions.toConnectionString $
                    options
                      { PostgresOptions.user = pure "normal_user"
                      , PostgresOptions.password = pure "password"
                      , PostgresOptions.dbname = pure "message_store"
                      }
             in ConnectionStrings{..}
      migrate options *> use tempMessageDb

    case result of
      Left err -> throwIO err
      Right value -> pure value
