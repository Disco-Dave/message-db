module MessageDb.Monad.Projection
  ( fetch
  , fetchWithSnapshots
  )
where

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Aeson as Aeson
import qualified Data.Pool as Pool
import MessageDb.Functions (BatchSize (FixedSize))
import MessageDb.Monad (MessageDbData (..), MonadMessageDb (..))
import MessageDb.Projection (Projected, Projection, SnapshotStreamName)
import qualified MessageDb.Projection as Projection
import qualified MessageDb.StreamName as Message


fetch ::
  ( MonadMessageDb m
  , MonadIO m
  ) =>
  Message.StreamName ->
  Projection state ->
  m (Maybe (Projected state))
fetch streamName projection = do
  MessageDbData{connectionPool, batchSize} <- getMessageDbData

  liftIO $
    Projection.fetch
      (Pool.withResource connectionPool)
      (FixedSize batchSize)
      streamName
      projection


fetchWithSnapshots ::
  ( MonadMessageDb m
  , MonadIO m
  , Aeson.ToJSON state
  , Aeson.FromJSON state
  ) =>
  Message.StreamName ->
  Projection state ->
  SnapshotStreamName ->
  m (Maybe (Projected state))
fetchWithSnapshots streamName projection snapshotStreamName = do
  MessageDbData{connectionPool, batchSize} <- getMessageDbData

  liftIO $
    Projection.fetchWithSnapshots
      (Pool.withResource connectionPool)
      (FixedSize batchSize)
      streamName
      projection
      snapshotStreamName
