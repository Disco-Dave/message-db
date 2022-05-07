module MessageDb.Projection.Handlers
  ( ProjectionHandlers,
    empty,
    attach,
    detach,
    handle,
  )
where

import Data.Aeson (FromJSON)
import MessageDb.Handlers (HandleError, Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import MessageDb.TypedMessage (TypedMessage)


type ProjectionHandlers entity = Handlers entity entity


empty :: ProjectionHandlers entity
empty = Handlers.empty


attach :: (FromJSON payload, FromJSON metadata) => MessageType -> (TypedMessage payload metadata -> entity -> entity) -> ProjectionHandlers entity -> ProjectionHandlers entity
attach =
  Handlers.attach



detach :: MessageType -> ProjectionHandlers entity -> ProjectionHandlers entity
detach =
  Handlers.detach


handle :: MessageType -> ProjectionHandlers entity -> Message -> entity -> Either HandleError entity
handle =
  Handlers.handle
