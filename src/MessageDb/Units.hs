module MessageDb.Units
  ( NumberOfMessages (..),
    Microseconds (..),
  )
where

import Numeric.Natural (Natural)


newtype NumberOfMessages = NumberOfMessages
  { fromNumberOfMessage :: Natural
  }
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)


newtype Microseconds = Microseconds
  { fromMicroseconds :: Natural
  }
  deriving (Show, Eq, Ord, Num, Real, Enum, Integral)
