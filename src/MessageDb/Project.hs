module MessageDb.Project (
  ProjectHandlers,
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

type ProjectHandlers entity = Handlers entity entity

emptyHandlers :: ProjectHandlers entity
emptyHandlers = Handlers.empty

attachHandler :: (FromJSON payload, FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> entity -> entity) -> ProjectHandlers entity -> ProjectHandlers entity
attachHandler =
  Handlers.attach

detachHandler :: MessageType -> ProjectHandlers entity -> ProjectHandlers entity
detachHandler =
  Handlers.detach

data Projection entity = Projection
  { initial :: entity
  , handlers :: ProjectHandlers entity
  }

project :: Projection entity -> NonEmpty Message -> Either HandleError entity
project Projection{..} messages = do
  let applyHandler entity message =
        let handle = Handlers.handle (Message.messageType message) handlers
         in case handle message entity of
              Right updatedEntity -> pure updatedEntity
              Left (MessageHandlerNotFound _) -> pure entity
              Left err -> Left err

  foldM applyHandler initial (toList messages)

fetch :: (forall a. (Postgres.Connection -> IO a) -> IO a) -> Functions.BatchSize -> StreamName -> Projection entity -> IO (Maybe entity)
fetch withConnection batchSize streamName projection =
  let query position currentProjection = do
        messages <- withConnection $ \connection ->
          Functions.getStreamMessages connection streamName (Just position) (Just batchSize) Nothing

        case messages of
          (firstMessage : otherMessages) -> do
            let nonEmptyMessages = firstMessage :| otherMessages

            entity <- either throwIO pure $ project currentProjection nonEmptyMessages

            if batchSize == Functions.Unlimited
              then pure $ Just entity
              else
                let nextPosition = streamPosition (NonEmpty.last nonEmptyMessages) + 1
                 in query nextPosition $ currentProjection{initial = entity}
          _ ->
            pure $
              if batchSize == Functions.Unlimited
                then Nothing
                else Just $ initial currentProjection
   in query 0 projection
