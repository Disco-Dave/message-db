-- | A projection is an aggregation of all messages in a stream.
module MessageDb.Projection
  ( Projection (..),
    Functions.BatchSize (..),
    UnprocessedMessage (..),
    Projected (..),
    versionIncludingUnprocessed,
    project,
    fetch,
  )
where

import Control.Exception (Exception)
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (HandleError (..))
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName)


-- | Defines how to perform a projection a stream.
data Projection state = Projection
  { initial :: state
  , handlers :: Handlers.ProjectionHandlers state
  }


-- | A message that was not able to be processed.
data UnprocessedMessage = UnprocessedMessage
  { message :: Message
  , reason :: HandleError
  }
  deriving (Show, Eq)


instance Exception UnprocessedMessage


-- | A projected state
data Projected state = Projected
  { unprocessed :: [UnprocessedMessage]
  , state :: state
  , version :: Functions.StreamVersion
  }
  deriving (Show, Eq, Functor)


-- | Constructs an empty projection.
empty :: state -> Projected state
empty initialState =
  Projected
    { unprocessed = []
    , state = initialState
    , version = Functions.DoesNotExist
    }


-- | Version of the projection with unprocessed messages included.
versionIncludingUnprocessed :: Projected state -> Functions.StreamVersion
versionIncludingUnprocessed Projected{..} =
  let unprocessedPositions =
        Functions.DoesExist . Message.messageStreamPosition . message <$> unprocessed

      allPositions =
        version :| unprocessedPositions
   in maximum allPositions


reverseUnprocessed :: Projected state -> Projected state
reverseUnprocessed projected =
  projected
    { unprocessed = reverse (unprocessed projected)
    }


project' :: Projection state -> NonEmpty Message -> Projected state
project' Projection{initial, handlers} messages =
  let applyHandler message projected@Projected{state, unprocessed} =
        case Handlers.handle handlers message of
          Right updateState ->
            projected
              { state = updateState state
              , version = Functions.DoesExist $ Message.messageStreamPosition message
              }
          Left newError ->
            projected
              { unprocessed = UnprocessedMessage message newError : unprocessed
              }
   in foldl' (flip applyHandler) (empty initial) (toList messages)


-- | Project a state of a stream by aggregating messages.
project :: Projection state -> NonEmpty Message -> Projected state
project messages =
  reverseUnprocessed . project' messages


-- | Query a stream and project the messages.
fetch :: forall state. Functions.WithConnection -> Functions.BatchSize -> StreamName -> Projection state -> IO (Maybe (Projected state))
fetch withConnection batchSize streamName projection =
  let query position projected@Projected{state, unprocessed} = do
        messages <- withConnection $ \connection ->
          Functions.getStreamMessages connection streamName (Just position) (Just batchSize) Nothing

        case messages of
          (firstMessage : otherMessages) -> do
            let nonEmptyMessages = firstMessage :| otherMessages

            let (Projected newErrors updatedState updatedVersion) =
                  project' (projection{initial = state}) nonEmptyMessages

                updatedProjectedState =
                  Projected
                    { unprocessed = newErrors <> unprocessed
                    , state = updatedState
                    , version = updatedVersion
                    }

            if batchSize == Functions.Unlimited
              then pure $ Just updatedProjectedState
              else
                let nextPosition = Message.messageStreamPosition (NonEmpty.last nonEmptyMessages) + 1
                 in query nextPosition updatedProjectedState
          _ ->
            pure $
              if position <= 0
                then Nothing
                else Just projected
   in fmap reverseUnprocessed <$> query 0 (empty $ initial projection)
