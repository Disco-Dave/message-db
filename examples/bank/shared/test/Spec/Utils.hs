module Spec.Utils
  ( testExamples
  )
where

import Data.Foldable (for_)
import Test.Hspec (Example (..), SpecWith, it)


testExamples
  :: ( Foldable t
     , Example assertion
     , Show example
     )
  => t example
  -> (example -> assertion)
  -> SpecWith (Arg assertion)
testExamples examples testExample =
  for_ examples $ \example ->
    it (show example) (testExample example)
