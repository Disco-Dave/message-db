module Main (main) where

import qualified Spec
import System.Timeout (timeout)
import Test.Hspec (around_, hspec, parallel)
import UnliftIO.Exception (throwString)


addTimeout :: IO () -> IO ()
addTimeout specItem = do
  result <- timeout 10_000_000 specItem

  case result of
    Nothing -> throwString "Spec item took too long!"
    Just () -> pure ()


main :: IO ()
main =
  hspec . parallel . around_ addTimeout $
    Spec.spec
