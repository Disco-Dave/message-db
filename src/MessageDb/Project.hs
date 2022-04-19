module MessageDb.Project (
  ProjectionHandlers,
  Projection (..),
  Functions.BatchSize (..),
  emptyHandlers,
  attachHandler,
  detachHandler,
  project,
  fetch,
) where

import Control.Exception (throwIO)
import Control.Monad (foldM)
import Data.Aeson (FromJSON)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (HandleError (..), Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message (streamPosition), MessageType)
import qualified MessageDb.Message as Message
import MessageDb.StreamName (StreamName)
import MessageDb.TypedMessage (TypedMessage)

type ProjectionHandlers entity = Handlers entity entity

data Projection entity = Projection
  { initial :: entity
  , handlers :: Handlers entity entity
  }

emptyHandlers :: ProjectionHandlers entity
emptyHandlers = Handlers.empty

attachHandler :: (FromJSON payload, FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> entity -> entity) -> ProjectionHandlers entity -> ProjectionHandlers entity
attachHandler =
  Handlers.attach

detachHandler :: MessageType -> ProjectionHandlers entity -> ProjectionHandlers entity
detachHandler =
  Handlers.detach

project :: Projection entity -> NonEmpty Message -> Either HandleError entity
project Projection{..} messages = do
  let applyHandler entity message =
        let handle = Handlers.handle (Message.messageType message) handlers
         in case handle message entity of
              Right updatedEntity -> pure updatedEntity
              Left (MessageHandlerNotFound _) -> pure entity
              Left err -> Left err

  foldM applyHandler initial (toList messages)

fetch :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Functions.BatchSize -> StreamName -> Projection entity -> IO entity
fetch withConnection batchSize streamName projection =
  let query position currentProjection = do
        messages <- withConnection $ \conn ->
          Functions.getStreamMessages conn streamName (Just position) (Just batchSize) Nothing

        case messages of
          (firstMessage : otherMessages) -> do
            let nonEmptyMessages = firstMessage :| otherMessages
                nextPosition = streamPosition (NonEmpty.last nonEmptyMessages) + 1

            nextEntity <- either throwIO pure $ project currentProjection nonEmptyMessages

            query nextPosition $
              currentProjection{initial = nextEntity}
          _ ->
            pure $ initial currentProjection
   in query 0 projection
