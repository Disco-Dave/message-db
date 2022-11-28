module MessageDb.Consumer.Snapshot
  ( Snapshot (..)
  , noSnapshot
  )
where

import MessageDb.Consumer.Projection (Projected)


data Snapshot m state = Snapshot
  { retrieveSnapshot :: m (Maybe (Projected state))
  , recordSnapshot :: Projected state -> m ()
  }


noSnapshot :: Applicative m => Snapshot m state
noSnapshot =
  Snapshot
    { retrieveSnapshot = pure Nothing
    , recordSnapshot = \_ -> pure ()
    }
