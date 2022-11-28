module MessageDb.Consumer.Subscription.SavedPosition
  ( SavedPosition (..)
  , noSavedPosition
  )
where

import MessageDb.Message.GlobalPosition (GlobalPosition)


data SavedPosition m = SavedPosition
  { restorePosition :: m (Maybe GlobalPosition)
  , savePosition :: GlobalPosition -> m ()
  }


noSavedPosition :: Applicative m => SavedPosition m
noSavedPosition =
  SavedPosition
    { restorePosition = pure Nothing
    , savePosition = \_ -> pure ()
    }
