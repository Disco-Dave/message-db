-- | Assigns message handlers per message type for performing projections.
module MessageDb.Projection.Handlers
  ( ProjectionHandlers,
    empty,
    attach,
    detach,
    handle,
    map,
  )
where

import Data.Aeson (FromJSON)
import MessageDb.Handlers (HandleError, Handlers)
import qualified MessageDb.Handlers as Handlers
import MessageDb.Message (Message, MessageType)
import MessageDb.TypedMessage (TypedMessage)
import Prelude hiding (map)


-- | Set of message handlers for a projection.
type ProjectionHandlers state = Handlers state state


-- | An empty set of projection handlers.
empty :: ProjectionHandlers state
empty = Handlers.empty


-- | Attach a function to handle a message type.
attach ::
  (FromJSON payload, FromJSON metadata) =>
  MessageType ->
  (TypedMessage payload metadata -> state -> state) ->
  ProjectionHandlers state ->
  ProjectionHandlers state
attach =
  Handlers.attach


-- | Detach a function for a message type.
detach :: MessageType -> ProjectionHandlers state -> ProjectionHandlers state
detach =
  Handlers.detach


-- | Run a handler for a message type.
handle :: ProjectionHandlers state -> Message -> state -> Either HandleError state
handle =
  Handlers.handle


-- | Map the state of the projection handlers from one type to another.
map :: (a -> b) -> (b -> a) -> ProjectionHandlers a -> ProjectionHandlers b
map aToB bToA =
  fmap $ \originalHandle message state ->
    aToB <$> originalHandle message (bToA state)
