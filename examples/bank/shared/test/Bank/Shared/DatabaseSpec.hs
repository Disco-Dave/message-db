module Bank.Shared.DatabaseSpec
  ( spec
  )
where

import Control.Monad.Reader (ReaderT (runReaderT))
import Database.PostgreSQL.Simple qualified as Postgres
import Bank.Shared.Database (DatabaseConfig (..))
import Bank.Shared.Database qualified as Database
import Bank.Shared.Database.IdleConnectionTimeout (IdleConnectionTimeout)
import Bank.Shared.Database.IdleConnectionTimeout qualified as IdleConnectionTimeout
import Bank.Shared.Database.MaxConnections (MaxConnections)
import Bank.Shared.Database.MaxConnections qualified as MaxConnections
import Spec.PostgresTemp (withConnectionUrl)
import Test.Hspec (Spec, aroundAll, describe, it, shouldBe)


unsafeIdleConnectionTimeout :: Double -> IO IdleConnectionTimeout
unsafeIdleConnectionTimeout raw = do
  Just value <- pure $ IdleConnectionTimeout.fromDouble raw
  pure value


unsafeMaxConnections :: Int -> IO MaxConnections
unsafeMaxConnections raw = do
  Just value <- pure $ MaxConnections.fromInt raw
  pure value


spec :: Spec
spec =
  aroundAll withConnectionUrl $ do
    describe "withConnectionPool" $
      it "can create a connection pool" $ \connectionUrl -> do
        idleConnectionTimeout <- unsafeIdleConnectionTimeout 15
        maxConnections <- unsafeMaxConnections 2

        let config =
              DatabaseConfig
                { connectionUrl
                , idleConnectionTimeout
                , maxConnections
                }

        Database.withConnectionPool config $ \_pool ->
          True `shouldBe` True

    describe "withConnection" $
      it "can use a connection from the pool" $ \connectionUrl -> do
        idleConnectionTimeout <- unsafeIdleConnectionTimeout 15
        maxConnections <- unsafeMaxConnections 2

        let config =
              DatabaseConfig
                { connectionUrl
                , idleConnectionTimeout
                , maxConnections
                }

        Database.withConnectionPool config $ \pool -> do
          [Postgres.Only number] <- flip runReaderT pool . Database.withConnection $ \connection ->
            Postgres.query_ @(Postgres.Only Integer) connection "select 3;"

          number `shouldBe` 3
