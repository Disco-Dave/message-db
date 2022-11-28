module MessageDb.Consumer.Snapshot.Message
  ( snapshotMessage
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Consumer.Projection (Projected (..))
import MessageDb.Consumer.Snapshot (Snapshot (..))
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message (messagePayload))
import MessageDb.Message.Payload (fromPayload)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.Produce (produce)
import MessageDb.Produce.Record (produceRecord)


retrieveSnapshotMessage
  :: ( Aeson.FromJSON state
     , MonadIO m
     )
  => Pool Postgres.Connection
  -> StreamName
  -> m (Maybe (Projected state))
retrieveSnapshotMessage connectionPool snapshotStreamName = do
  maybeMessage <-
    liftIO . Pool.withResource connectionPool $ \connection ->
      Functions.getLastStreamMessage connection snapshotStreamName

  pure $ case fmap (fromPayload . messagePayload) maybeMessage of
    Just (Right projectedState) ->
      Just projectedState
    _ ->
      Nothing


recordSnapshotMessage
  :: ( Aeson.ToJSON state
     , MonadIO m
     )
  => Pool Postgres.Connection
  -> StreamName
  -> Projected state
  -> m ()
recordSnapshotMessage connectionPool snapshotStreamName projectedState = do
  oldProjectedState <- retrieveSnapshotMessage @Aeson.Value connectionPool snapshotStreamName

  let oldVersion = fmap projectedStreamVersion oldProjectedState
      newVersion = projectedStreamVersion projectedState

  when (oldVersion /= Just newVersion) $ do
    let record =
          produceRecord snapshotStreamName "ProjectedStateSnapshotted" projectedState

    void . liftIO . Pool.withResource connectionPool $ \connection ->
      produce connection record


snapshotMessage
  :: ( Aeson.ToJSON state
     , Aeson.FromJSON state
     , MonadIO m
     )
  => Pool Postgres.Connection
  -> StreamName
  -> Snapshot m state
snapshotMessage connectionPool streamName =
  Snapshot
    { retrieveSnapshot = retrieveSnapshotMessage connectionPool streamName
    , recordSnapshot = recordSnapshotMessage connectionPool streamName
    }
