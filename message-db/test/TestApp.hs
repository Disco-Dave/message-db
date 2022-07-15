module TestApp
  ( TestAppData (..),
    withTestAppData,
    TestApp (..),
    runWith,
    run,
    withConnection,
    withSubscriptions,
    blockUntilStreamHas,
    blockUntilCategoryHas,
  )
where

import Control.Monad.Cont (ContT (ContT, runContT))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT))
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import MessageDb.StreamName (Category, StreamName)
import MessageDb.Subscription (Subscription)
import qualified MessageDb.Subscription as Subscription
import MessageDb.Temp (ConnectionStrings (..))
import qualified MessageDb.Temp as TempMessageDb
import MessageDb.Units (NumberOfMessages (NumberOfMessages))
import UnliftIO (MonadUnliftIO (withRunInIO))
import qualified UnliftIO.Async as Async
import UnliftIO.Concurrent (threadDelay)
import UnliftIO.Exception (bracket)


newtype TestAppData = TestAppData
  { connectionPool :: Pool Postgres.Connection
  }


withTestAppData :: (TestAppData -> IO value) -> IO value
withTestAppData =
  runContT $ do
    ConnectionStrings{normalConnectionString} <-
      ContT TempMessageDb.withConnectionStrings

    connectionPool <-
      let createPool =
            let createConnection = Postgres.connectPostgreSQL normalConnectionString
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
    , MonadFail
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


withSubscriptions :: [TestApp Subscription] -> (TestAppData -> IO ()) -> IO ()
withSubscriptions makeSubscriptions useTestAppData = run $ do
  testAppData@TestAppData{connectionPool} <- ask

  subscriptions <-
    let startSubscription = liftIO . Subscription.start (Pool.withResource connectionPool)
        startAll = Async.mapConcurrently_ startSubscription
     in startAll <$> sequenceA makeSubscriptions

  Async.withAsync subscriptions $ \task -> do
    Async.link task
    liftIO $ useTestAppData testAppData


blockUntilStreamHas :: StreamName -> NumberOfMessages -> TestApp ()
blockUntilStreamHas streamName numberOfMessages = do
  messages <- withConnection $ \connection ->
    liftIO $
      Functions.getStreamMessages
        connection
        streamName
        Nothing
        Nothing
        Nothing

  let currentNumberOfMessages = NumberOfMessages . fromIntegral $ length messages

  if currentNumberOfMessages < numberOfMessages
    then do
      threadDelay 100_000
      blockUntilStreamHas streamName numberOfMessages
    else pure ()


blockUntilCategoryHas :: Category -> NumberOfMessages -> TestApp ()
blockUntilCategoryHas categoryName numberOfMessages = do
  messages <- withConnection $ \connection ->
    liftIO $
      Functions.getCategoryMessages
        connection
        categoryName
        Nothing
        Nothing
        Nothing
        Nothing
        Nothing

  let currentNumberOfMessages = NumberOfMessages . fromIntegral $ length messages

  if currentNumberOfMessages < numberOfMessages
    then do
      threadDelay 50_000
      blockUntilCategoryHas categoryName numberOfMessages
    else pure ()
