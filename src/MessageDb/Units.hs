-- | Different units of measures use throughout the library.
module MessageDb.Units
  ( NumberOfMessages (..)
  , Microseconds (..)
  )
where

import Numeric.Natural (Natural)


-- | A number of messages. Must be 0 or greater.
newtype NumberOfMessages = NumberOfMessages
  { numberOfMessagesToNatural :: Natural
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural


-- | Time in microseconds. Must be 0 or greater.
newtype Microseconds = Microseconds
  { microsecondsToNatural :: Natural
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural
