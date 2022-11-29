module Bank.Shared.Database.MaxConnections
  ( MaxConnections
  , fromInt
  , toInt
  , def
  )
where

import Data.Coerce (coerce)


newtype MaxConnections = MaxConnections Int
  deriving (Show, Eq)


fromInt :: Int -> Maybe MaxConnections
fromInt int
  | int > 0 = Just $ MaxConnections int
  | otherwise = Nothing


-- | Downgrade the 'ConnectionUrl' to an unvalidated 'ByteString'.
toInt :: MaxConnections -> Int
toInt =
  coerce


def :: MaxConnections
def =
  MaxConnections 5
