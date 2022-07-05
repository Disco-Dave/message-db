-- | Different units of measures used throughout the library that I wasn't able to find a better home for.
module MessageDb.Units
  ( NumberOfMessages (..)
  , Microseconds (..)
  )
where

import Numeric.Natural (Natural)


-- | A number of messages. Must be 0 or greater.
newtype NumberOfMessages = NumberOfMessages
  { numberOfMessagesToNatural :: Natural
  -- ^ Convert 'NumberOfMessages' to a 'Natural'.
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural


-- | Time in microseconds. Must be 0 or greater.
newtype Microseconds = Microseconds
  { microsecondsToNatural :: Natural
  -- ^ Convert 'Microseconds' to a 'Natural'.
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural
