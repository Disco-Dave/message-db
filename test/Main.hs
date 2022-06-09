module Main (main) where

import qualified Spec
import System.Timeout (timeout)
import Test.Hspec (around_, hspec, parallel)
import Control.Monad

main :: IO ()
main = hspec . parallel . around_ (void . timeout 10_000_000) $ 
  Spec.spec
