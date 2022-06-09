module Generators.StreamName
  ( genIdentityName,
    genCategoryName,
    genStreamName,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MessageDb.StreamName (StreamName (..))
import qualified MessageDb.StreamName as StreamName


genIdentityName :: Gen StreamName.IdentityName
genIdentityName =
  let range = Range.linear 5 50
      validCharacters =
        Gen.frequency
          [ (25, Gen.alphaNum)
          , (1, pure '-')
          ]
   in StreamName.IdentityName <$> Gen.text range validCharacters


genCategoryName :: Gen StreamName.CategoryName
genCategoryName =
  let range = Range.linear 5 50
      validCharacters =
        Gen.frequency
          [ (30, Gen.alphaNum)
          , (1, pure ':')
          ]
   in StreamName.categoryName <$> Gen.text range validCharacters


genStreamName :: Gen StreamName
genStreamName = do
  StreamName.addIdentityToCategory
    <$> genCategoryName
    <*> genIdentityName
