-- | Strategies for saving subscription position.
module MessageDb.Subscription.PositionStrategy
  ( LastPositionSaved,
    CurrentPosition,
    PositionSaved,
    PositionStrategy (..),
    dontSave,
    PositionUpdateInterval (..),
    writeToStream,
  )
where

import Control.Monad (void)
import Data.Coerce (coerce)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName)
import MessageDb.Units (NumberOfMessages (..))
import Numeric.Natural (Natural)


type LastPositionSaved = Message.GlobalPosition

type CurrentPosition = Message.GlobalPosition

type PositionSaved = Message.GlobalPosition


-- | Strategy for saving and restoring a subscription's position.
data PositionStrategy = PositionStrategy
  { restore :: IO Message.GlobalPosition
  , save :: LastPositionSaved -> CurrentPosition -> IO (Maybe PositionSaved)
  }


-- | Start at zero and don't ever save the position.
dontSave :: PositionStrategy
dontSave =
  PositionStrategy
    { restore = pure 0
    , save = \_ _ -> pure Nothing
    }


-- | Minimum difference between the current position and last position saved to save the position.
newtype PositionUpdateInterval = PositioUpdateInterval
  { fromPositionUpdateInterval :: NumberOfMessages
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via NumberOfMessages


-- | Write the subscription's position to a stream.
writeToStream :: Functions.WithConnection -> PositionUpdateInterval -> StreamName -> PositionStrategy
writeToStream withConnection positionUpdateInterval streamName =
  let messageType :: Message.MessageType
      messageType =
        "GlobalPositionSaved"

      savePosition :: Postgres.Connection -> Message.GlobalPosition -> IO ()
      savePosition connection position =
        void $
          Functions.writeMessage @Message.GlobalPosition @()
            connection
            streamName
            messageType
            position
            Nothing
            Nothing

      restore :: IO Message.GlobalPosition
      restore = do
        maybeMessage <- withConnection $ \connection ->
          Functions.getLastStreamMessage connection streamName

        pure $ case fmap (Message.parsePayload . Message.messagePayload) maybeMessage of
          Just (Right position) -> position
          _ -> 0

      save :: LastPositionSaved -> CurrentPosition -> IO (Maybe PositionSaved)
      save lastPositionSaved currentPosition =
        let interval = fromIntegral $ coerce @_ @Natural positionUpdateInterval
            difference = Message.globalPositionToInteger $ currentPosition - lastPositionSaved
         in if difference < interval
              then pure Nothing
              else do
                withConnection $ \connection ->
                  savePosition connection currentPosition

                pure $ Just currentPosition
   in PositionStrategy{..}
