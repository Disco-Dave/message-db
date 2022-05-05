module MessageDb.Projection
  ( ProjectHandlers,
    Projection (..),
    Functions.BatchSize (..),
    UnprocessedMessage (..),
    emptyHandlers,
    attachHandler,
    detachHandler,
    project,
    fetch,
  )
where

import Control.Exception (Exception)
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


data UnprocessedMessage = UnprocessedMessage
  { message :: Message
  , reason :: HandleError
  }
  deriving (Show, Eq)
instance Exception UnprocessedMessage


project :: Projection entity -> NonEmpty Message -> ([UnprocessedMessage], entity)
project Projection{..} messages =
  let applyHandler message (errors, entity) =
        let handle = Handlers.handle (Message.messageType message) handlers
         in case handle message entity of
              Right updatedEntity -> (errors, updatedEntity)
              Left newError -> (UnprocessedMessage message newError : errors, entity)
   in foldr applyHandler ([], initial) (toList messages)


fetch :: Functions.WithConnection -> Functions.BatchSize -> StreamName -> Projection entity -> IO (Maybe ([UnprocessedMessage], entity))
fetch withConnection batchSize streamName projection =
  let query position (errors, entity) = do
        messages <- withConnection $ \connection ->
          Functions.getStreamMessages connection streamName (Just position) (Just batchSize) Nothing

        case messages of
          (firstMessage : otherMessages) -> do
            let nonEmptyMessages = firstMessage :| otherMessages

            let (newErrors, updatedEntity) = project (projection{initial = entity}) nonEmptyMessages

            if batchSize == Functions.Unlimited
              then pure $ Just (errors <> newErrors, updatedEntity)
              else
                let nextPosition = 1 + streamPosition (NonEmpty.last nonEmptyMessages)
                 in query nextPosition (errors <> newErrors, updatedEntity)
          _ ->
            pure $
              if batchSize == Functions.Unlimited
                then Nothing
                else Just (errors, entity)
   in query 0 ([], initial projection)
