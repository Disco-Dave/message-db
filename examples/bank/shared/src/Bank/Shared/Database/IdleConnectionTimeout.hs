module Bank.Shared.Database.IdleConnectionTimeout
  ( IdleConnectionTimeout
  , fromDouble
  , toDouble
  , def
  )
where

import Data.Coerce (coerce)


newtype IdleConnectionTimeout = IdleConnectionTimeout Double
  deriving (Show, Eq)


fromDouble :: Double -> Maybe IdleConnectionTimeout
fromDouble double
  | double >= 0.5 = Just $ IdleConnectionTimeout double
  | otherwise = Nothing


toDouble :: IdleConnectionTimeout -> Double
toDouble =
  coerce


def :: IdleConnectionTimeout
def =
  IdleConnectionTimeout 15
