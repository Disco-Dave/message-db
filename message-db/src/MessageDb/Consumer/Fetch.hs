module MessageDb.Consumer.Fetch where

import Control.Monad.Except (MonadIO)
import Data.Pool (Pool)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Consumer.BatchSize (BatchSize)
import MessageDb.Consumer.Projection (Projected, Projection)
import MessageDb.Message.StreamName (StreamName)


data FetchSnapshot m state = FetchSnapshot
  { retrieveSnapshot :: m (Maybe (Projected state))
  , recordSnapshot :: Projected state -> m ()
  }


data FetchOptions m state = FetchOptions
  { fetchPool :: Pool Postgres.Connection
  , fetchBatchSize :: BatchSize
  , fetchStreamName :: StreamName
  , fetchProjection :: Projection state
  , fetchSnapshot :: Maybe (FetchSnapshot m state)
  }


fetch :: MonadIO m => FetchOptions m state -> m (Maybe (Projected state))
fetch = undefined
