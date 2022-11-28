module MessageDb.Consumer.Fetch
  ( Fetch (..)
  , fetch
  )
where

import Control.Monad (join)
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
import MessageDb.Consumer.Snapshot (Snapshot (..))
import qualified MessageDb.Functions as Functions
import MessageDb.Message.StreamName (StreamName)
import qualified MessageDb.StreamVersion as StreamVersion


data Fetch m state = Fetch
  { fetchConnectionPool :: Pool Postgres.Connection
  , fetchBatchSize :: Maybe BatchSize
  , fetchStreamName :: StreamName
  , fetchProjection :: Projection state
  , fetchSnapshot :: Maybe (Snapshot m state)
  , fetchCondition :: Maybe Condition
  }


fetch :: MonadIO m => Fetch m state -> m (Maybe (Projected state))
fetch Fetch{..} = do
  snapshot <- join <$> traverse retrieveSnapshot fetchSnapshot

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
              fetchBatchSize
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

            for_ fetchSnapshot $ \Snapshot{recordSnapshot} ->
              recordSnapshot nextProjected

            case fetchBatchSize of
              Just BatchSize.Unlimited ->
                pure $ Just nextProjected
              _ ->
                loop nextProjected

  loop initialProjected
