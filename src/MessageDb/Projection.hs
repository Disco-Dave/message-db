module MessageDb.Projection
  ( Projection (..),
    Functions.BatchSize (..),
    UnprocessedMessage (..),
    project,
    fetch,
  )
where

import Control.Exception (Exception)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import qualified Data.List.NonEmpty as NonEmpty
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers (HandleError (..))
import MessageDb.Message (Message (streamPosition))
import qualified MessageDb.Message as Message
import MessageDb.Projection.Handlers (ProjectionHandlers)
import qualified MessageDb.Projection.Handlers as ProjectionHandlers
import MessageDb.StreamName (StreamName)


data Projection entity = Projection
  { initial :: entity
  , handlers :: ProjectionHandlers entity
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
        let handle = ProjectionHandlers.handle (Message.messageType message) handlers
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
