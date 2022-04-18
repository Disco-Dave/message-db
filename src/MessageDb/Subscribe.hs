module MessageDb.Subscribe (
  SubscriberId (..),
  StartPosition (..),
  NumberOfMessages (..),
  Microseconds (..),
  Subscription (..),
  start,
) where

import Control.Concurrent (threadDelay)
import qualified Control.Immortal as Immortal
import Control.Monad (void)
import Data.String (IsString)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple as Postgres
import MessageDb.Functions ()
import MessageDb.Handlers (Handlers, NoState)
import MessageDb.Message (CategoryName, GlobalPosition)
import MessageDb.StreamName ()
import Numeric.Natural (Natural)

newtype SubscriberId = SubscriberId
  { fromSubscriberId :: Text
  }
  deriving (Show, Eq, Ord, IsString)

data StartPosition
  = RestorePosition
  | SpecificPosition GlobalPosition

newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Show, Eq, Ord, Num)

newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Show, Eq, Ord, Num)

data Subscription = Subscription
  { subscriberId :: SubscriberId
  , categoryName :: CategoryName
  , startPosition :: StartPosition
  , messagesPerTick :: NumberOfMessages
  , positionUpdateInterval :: NumberOfMessages
  , tickInterval :: Microseconds
  , handlers :: Handlers NoState (IO ())
  }

poll :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
poll withConnection Subscription{..} = do
  threadDelay (fromIntegral (fromMicroseconds tickInterval))

start :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Subscription -> IO ()
start withConnection subscription =
  void . Immortal.create $ \_ ->
    poll withConnection subscription
