module Main (main) where

import Spec qualified
import System.Timeout (timeout)
import Test.Hspec (around_, hspec, parallel)


addTimeout :: IO () -> IO ()
addTimeout specItem = do
  result <- timeout 10_000_000 specItem

  case result of
    Nothing -> fail "Spec item took longer than 10 seconds."
    Just () -> pure ()


main :: IO ()
main =
  hspec . parallel . around_ addTimeout $
    Spec.spec
