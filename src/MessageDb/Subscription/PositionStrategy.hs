module MessageDb.Subscription.PositionStrategy
  ( LastPositionSaved,
    CurrentPosition,
    PositionSaved,
    PositionStrategy (..),
    dontSave,
    PositionUpdateInterval,
    writeToStream,
  )
where

import Control.Monad (void)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import MessageDb.Units (NumberOfMessages (..))


type LastPositionSaved = Message.GlobalPosition
type CurrentPosition = Message.GlobalPosition
type PositionSaved = Message.GlobalPosition


data PositionStrategy = PositionStrategy
  { restore :: IO Message.GlobalPosition
  , save :: LastPositionSaved -> CurrentPosition -> IO (Maybe PositionSaved)
  }


dontSave :: PositionStrategy
dontSave =
  PositionStrategy
    { restore = pure 0
    , save = \_ _ -> pure Nothing
    }


type PositionUpdateInterval = NumberOfMessages


writeToStream :: Functions.WithConnection -> PositionUpdateInterval -> Message.StreamName -> PositionStrategy
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

        pure $ case fmap Message.typedPayload maybeMessage of
          Just (Right position) -> position
          _ -> 0

      save :: LastPositionSaved -> CurrentPosition -> IO (Maybe PositionSaved)
      save lastPositionSaved currentPosition =
        let interval = fromIntegral $ fromNumberOfMessage positionUpdateInterval
            difference = Message.fromGlobalPosition $ currentPosition - lastPositionSaved
         in if difference < interval
              then pure Nothing
              else do
                withConnection $ \connection ->
                  savePosition connection currentPosition

                pure $ Just currentPosition
   in PositionStrategy{..}
