module Spec.PostgresTemp
  ( withConnectionUrl
  )
where

import Database.Postgres.Temp qualified as PostgresTemp
import Bank.Shared.Database.ConnectionUrl (ConnectionUrl)
import Bank.Shared.Database.ConnectionUrl qualified as ConnectionUrl
import UnliftIO.Exception (throwIO)


withConnectionUrl :: (ConnectionUrl -> IO a) -> IO a
withConnectionUrl use = do
  result <- PostgresTemp.with $ \db -> do
    Just url <- pure . ConnectionUrl.fromByteString $ PostgresTemp.toConnectionString db
    use url

  either throwIO pure result
