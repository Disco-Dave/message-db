module MessageDb.Consumer.Snapshots
  ( Snapshots (..)
  , noSnapshots
  , messageSnapshots
  )
where

import Control.Monad (void, when)
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Consumer.Projection.Projected (Projected (..))
import qualified MessageDb.Functions as Functions
import MessageDb.Message (Message (..))
import MessageDb.Message.Payload (fromPayload)
import MessageDb.Message.StreamName (StreamName)
import MessageDb.Produce (produce)
import MessageDb.Produce.Record (produceRecord)


data Snapshots m state = Snapshots
  { retrieveSnapshot :: m (Maybe (Projected state))
  , recordSnapshot :: Projected state -> m ()
  }


noSnapshots :: Applicative m => Snapshots m state
noSnapshots =
  Snapshots
    { retrieveSnapshot = pure Nothing
    , recordSnapshot = \_ -> pure ()
    }


messageSnapshots
  :: forall state m
   . ( Aeson.ToJSON state
     , Aeson.FromJSON state
     , MonadIO m
     )
  => Pool Postgres.Connection
  -> StreamName
  -> Snapshots m state
messageSnapshots connectionPool snapshotStreamName = do
  let retrieveSnapshotMessage :: forall json. Aeson.FromJSON json => m (Maybe (Projected json))
      retrieveSnapshotMessage = do
        maybeMessage <-
          liftIO . Pool.withResource connectionPool $ \connection ->
            Functions.getLastStreamMessage connection snapshotStreamName

        pure $ case fmap (fromPayload . messagePayload) maybeMessage of
          Just (Right projectedState) ->
            Just projectedState
          _ ->
            Nothing

      recordSnapshotMessage projectedState = do
        oldProjectedState <- retrieveSnapshotMessage @Aeson.Value

        let oldVersion = fmap projectedStreamVersion oldProjectedState
            newVersion = projectedStreamVersion projectedState

        when (oldVersion /= Just newVersion) $ do
          let record = produceRecord snapshotStreamName  projectedState
           in void . liftIO . Pool.withResource connectionPool $ \connection ->
                produce connection record
   in Snapshots
        { retrieveSnapshot = retrieveSnapshotMessage
        , recordSnapshot = recordSnapshotMessage
        }
