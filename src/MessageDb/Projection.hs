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
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import MessageDb.Projection.Handlers (ProjectionHandlers)
import qualified MessageDb.Projection.Handlers as ProjectionHandlers
import MessageDb.StreamName (StreamName)


data Projection state = Projection
  { initial :: state
  , handlers :: ProjectionHandlers state
  }


data UnprocessedMessage = UnprocessedMessage
  { message :: Message
  , reason :: HandleError
  }
  deriving (Show, Eq)
instance Exception UnprocessedMessage


data Projected state = Projected
  { unprocessed :: [UnprocessedMessage]
  , state :: state
  , version :: Functions.StreamVersion
  }
  deriving (Show, Eq)


empty :: state -> Projected state
empty initialState =
  Projected
    { unprocessed = []
    , state = initialState
    , version = Functions.DoesNotExist
    }


versionIncludingUnprocessed :: Projected state -> Functions.StreamVersion
versionIncludingUnprocessed Projected{..} =
  let unprocessedPositions =
        Functions.DoesExist . Message.streamPosition . message <$> unprocessed

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
        let handle = ProjectionHandlers.handle (Message.messageType message) handlers
         in case handle message state of
              Right updatedState ->
                projected
                  { state = updatedState
                  , version = Functions.DoesExist $ Message.streamPosition message
                  }
              Left newError ->
                projected
                  { unprocessed = UnprocessedMessage message newError : unprocessed
                  }
   in foldl' (flip applyHandler) (empty initial) (toList messages)


project :: Projection state -> NonEmpty Message -> Projected state
project messages =
  reverseUnprocessed . project' messages


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
                let nextPosition = Message.streamPosition (NonEmpty.last nonEmptyMessages) + 1
                 in query nextPosition updatedProjectedState
          _ ->
            pure $
              if position <= 0
                then Nothing
                else Just projected
   in fmap reverseUnprocessed <$> query 0 (empty $ initial projection)
