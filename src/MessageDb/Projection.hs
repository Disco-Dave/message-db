-- | A projection is an aggregation of all messages in a stream.
module MessageDb.Projection
  ( Projection (..),
    Functions.BatchSize (..),
    UnprocessedMessage (..),
    Projected (..),
    versionIncludingUnprocessed,
    project,
    fetch,
    SnapshotStreamName (..),
    fetchWithSnapshots,
  )
where

import Control.Exception (Exception)
import Control.Exception.Safe (handle)
import Control.Monad (void)
import Control.Monad.Except (liftEither)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Class (MonadTrans (lift))
import Control.Monad.Trans.Except (runExceptT)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import qualified Data.Aeson as Aeson
import Data.Bifunctor (Bifunctor (first))
import Data.Coerce (coerce)
import Data.Foldable (foldl', toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import Data.String (IsString)
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
        case Handlers.projectionHandle handlers message state of
          Right updatedState ->
            projected
              { state = updatedState
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
fetch ::
  forall state.
  Functions.WithConnection ->
  Functions.BatchSize ->
  StreamName ->
  Message.StreamPosition ->
  Projection state ->
  IO (Maybe (Projected state))
fetch withConnection batchSize streamName startingPosition projection =
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
              if position <= startingPosition
                then Nothing
                else Just projected
   in fmap reverseUnprocessed <$> query startingPosition (empty $ initial projection)


data Snapshot state = Snapshot
  { snapshotState :: state
  , snapshotPosition :: Message.StreamPosition
  , snapshotVersion :: Message.StreamPosition
  }


newtype SnapshotStreamName = SnapshotStreamName
  { fromSnapshotStreamName :: StreamName
  }
  deriving (Show, Eq, Ord, IsString, Semigroup)


retrieveSnapshot :: Aeson.FromJSON state => Functions.WithConnection -> SnapshotStreamName -> IO (Maybe (Either UnprocessedMessage (Snapshot state)))
retrieveSnapshot withConnection streamName =
  runMaybeT . runExceptT $ do
    message <-
      lift . MaybeT . withConnection $ \connection ->
        Functions.getLastStreamMessage connection (coerce streamName)

    Message.ParsedMessage{parsedPayload, parsedMetadata} <-
      let handleError failure =
            UnprocessedMessage
              { message = message
              , reason = Handlers.HandlerParseFailure failure
              }
       in liftEither . first handleError $ Message.parseMessage message

    pure $
      Snapshot
        { snapshotState = parsedPayload
        , snapshotPosition = parsedMetadata
        , snapshotVersion = Message.messageStreamPosition message
        }


recordSnapshot :: forall state. Aeson.ToJSON state => Functions.WithConnection -> SnapshotStreamName -> state -> Message.StreamPosition -> Functions.ExpectedVersion -> IO ()
recordSnapshot withConnection streamName snapshotState snapshotPosition expectedVersion = do
  liftIO . withConnection $ \connection ->
    -- An 'Functions.ExpectedVersionViolation' means that someone else wrote a snapshot
    -- in between us reading the latest snapshot and computing the next snapshot.
    -- In this case, we disregard this snapshot because it's out of date.
    handle (\(_ :: Functions.ExpectedVersionViolation) -> pure ()) . void $
      Functions.writeMessage
        connection
        (coerce streamName)
        "Snapshotted"
        snapshotState
        (Just snapshotPosition)
        (Just expectedVersion)


fetchWithSnapshots ::
  forall state.
  (Aeson.ToJSON state, Aeson.FromJSON state) =>
  Functions.WithConnection ->
  Functions.BatchSize ->
  StreamName ->
  SnapshotStreamName ->
  Projection state ->
  IO (Maybe (Projected state))
fetchWithSnapshots withConnection batchSize streamName snapshotStreamName projection = do
  previousSnapshotResult <- retrieveSnapshot @state withConnection snapshotStreamName

  fetchResult <-
    uncurry (fetch @state withConnection batchSize streamName) $
      case previousSnapshotResult of
        Just (Right Snapshot{..}) ->
          (snapshotPosition + 1, projection{initial = snapshotState})
        _ ->
          (0, projection)

  case fetchResult of
    Just Projected{version, state}
      | Functions.DoesExist updatedSnapshotVersion <- version ->
          let expectedVersion =
                Functions.ExpectedVersion $ case previousSnapshotResult of
                  Just (Left UnprocessedMessage{message}) ->
                    Functions.DoesExist $ Message.messageStreamPosition message
                  Just (Right Snapshot{snapshotVersion}) ->
                    Functions.DoesExist snapshotVersion
                  Nothing ->
                    Functions.DoesNotExist
           in recordSnapshot withConnection snapshotStreamName state updatedSnapshotVersion expectedVersion
    _ -> pure ()

  pure $ case previousSnapshotResult of
    (Just (Left err)) ->
      fmap (\p -> p{unprocessed = err : unprocessed p}) fetchResult
    _ ->
      fetchResult
