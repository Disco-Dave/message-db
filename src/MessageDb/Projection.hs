module MessageDb.Projection
  ( ProjectHandlers,
    Projection (..),
    Functions.BatchSize (..),
    emptyHandlers,
    attachHandler,
    detachHandler,
    project,
    fetch,
  )
where

import Data.Aeson (FromJSON)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
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


project :: Projection entity -> NonEmpty Message -> ([HandleError], entity)
project Projection{..} messages =
  let applyHandler message (errors, entity) =
        let handle = Handlers.handle (Message.messageType message) handlers
         in case handle message entity of
              Right updatedEntity -> (errors, updatedEntity)
              Left newError -> (newError : errors, entity)
   in foldr applyHandler ([], initial) (toList messages)


fetch :: Functions.WithConnection -> Functions.BatchSize -> StreamName -> Projection entity -> IO (Maybe ([HandleError], entity))
fetch withConnection batchSize streamName projection =
  let query position (errors, entity) = do
        messages <- withConnection $ \connection ->
          Functions.getStreamMessages connection streamName (Just position) (Just batchSize) Nothing

        case messages of
          (firstMessage : otherMessages) -> do
            let nonEmptyMessages = firstMessage :| otherMessages

            let (updatedErrors, updatedEntity) = project (projection{initial = entity}) nonEmptyMessages

            if batchSize == Functions.Unlimited
              then pure $ Just (updatedErrors, updatedEntity)
              else
                let nextPosition = streamPosition (NonEmpty.last nonEmptyMessages)
                 in query nextPosition (updatedErrors, updatedEntity)
          _ ->
            pure $
              if batchSize == Functions.Unlimited
                then Nothing
                else Just (errors, entity)
   in query 0 ([], initial projection)
