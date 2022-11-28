module MessageDb.Consumer.Fetch
  ( FetchParams (..)
  , fetchParams
  , fetch
  )
where

import Control.Monad.Except (MonadIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Foldable (for_)
import Data.List.NonEmpty (nonEmpty)
import Data.Maybe (fromMaybe)
import Data.Pool (Pool)
import qualified Data.Pool as Pool
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Consumer.BatchSize (BatchSize)
import qualified MessageDb.Consumer.BatchSize as BatchSize
import MessageDb.Consumer.Condition (Condition)
import MessageDb.Consumer.Projection
  ( Projected (..)
  , Projection (..)
  , emptyProjection
  , project
  )
import MessageDb.Consumer.Snapshots (Snapshots (..), noSnapshots)
import qualified MessageDb.Functions as Functions
import MessageDb.Message.StreamName (StreamName)
import qualified MessageDb.StreamVersion as StreamVersion


data FetchParams m state = FetchParams
  { fetchConnectionPool :: Pool Postgres.Connection
  , fetchBatchSize :: BatchSize
  , fetchStreamName :: StreamName
  , fetchProjection :: Projection state
  , fetchSnapshot :: Snapshots m state
  , fetchCondition :: Maybe Condition
  }


fetchParams :: Applicative m => Pool Postgres.Connection -> StreamName -> Projection state -> FetchParams m state
fetchParams connectionPool streamName projection =
  FetchParams
    { fetchConnectionPool = connectionPool
    , fetchBatchSize = BatchSize.FixedSize 100
    , fetchStreamName = streamName
    , fetchProjection = projection
    , fetchSnapshot = noSnapshots
    , fetchCondition = Nothing
    }


fetch :: MonadIO m => FetchParams m state -> m (Maybe (Projected state))
fetch FetchParams{..} = do
  snapshot <- retrieveSnapshot fetchSnapshot

  let initialProjected =
        fromMaybe (emptyProjection (projectionState fetchProjection)) snapshot

      loop !currentProjected = do
        let currentStreamVersion =
              projectedStreamVersion currentProjected

            queryPosition =
              case currentStreamVersion of
                StreamVersion.DoesNotExist -> Nothing
                StreamVersion.DoesExist position -> Just $ position + 1

            currentProjection =
              fetchProjection
                { projectionState = projectedState currentProjected
                }

        maybeMessages <-
          liftIO . fmap nonEmpty . Pool.withResource fetchConnectionPool $ \connection ->
            Functions.getStreamMessages
              connection
              fetchStreamName
              queryPosition
              (Just fetchBatchSize)
              fetchCondition

        case (maybeMessages, currentStreamVersion) of
          (Nothing, StreamVersion.DoesNotExist) ->
            pure Nothing
          (Nothing, StreamVersion.DoesExist _) ->
            pure $ Just currentProjected
          (Just messages, _) -> do
            let nextProjected =
                  let newProjected = project currentProjection messages
                   in newProjected
                        { projectedErrors =
                            projectedErrors newProjected <> projectedErrors currentProjected
                        }

            case fetchBatchSize of
              BatchSize.Unlimited ->
                pure $ Just nextProjected
              _ ->
                loop nextProjected

  finalProjected <- loop initialProjected

  for_ finalProjected $
    recordSnapshot fetchSnapshot

  pure finalProjected
