module MessageDb.Units
  ( NumberOfMessages (..),
    Microseconds (..),
  )
where

import Numeric.Natural (Natural)


newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural


newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Eq, Ord, Num, Real, Enum, Integral)
  deriving (Show) via Natural
