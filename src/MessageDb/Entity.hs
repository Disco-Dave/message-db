module MessageDb.Entity (
  Entity (..),
  combine,
) where

-- | An entity is a structure that is the result of combining one or more events from a stream.
class Entity entity where
  -- Event initializing the stream
  type InitialEvent entity

  -- | An event represents a fact in the past.
  type Event entity

  -- | Initializes the default values for an entity
  initialize :: InitialEvent entity -> entity

  -- | Update an entity by applying an event to it
  update :: Event entity -> entity -> entity

-- | Combine all entity messages in order to compute the final entity
combine :: Entity entity => InitialEvent entity -> [Event entity] -> entity
combine initialEvent =
  foldr update (initialize initialEvent)
