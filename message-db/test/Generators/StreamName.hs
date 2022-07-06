module Generators.StreamName
  ( genIdentifier
  , genCategory
  , genStreamName
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MessageDb.StreamName (StreamName (..))
import qualified MessageDb.StreamName as StreamName


genIdentifier :: Gen StreamName.Identifier
genIdentifier =
  let range = Range.linear 5 50
      validCharacters =
        Gen.frequency
          [ (25, Gen.alphaNum)
          , (1, pure '-')
          ]
   in StreamName.Identifier <$> Gen.text range validCharacters


genCategory :: Gen StreamName.Category
genCategory =
  let range = Range.linear 5 50
      validCharacters =
        Gen.frequency
          [ (30, Gen.alphaNum)
          , (1, pure ':')
          ]
   in StreamName.category <$> Gen.text range validCharacters


genStreamName :: Gen StreamName
genStreamName = do
  StreamName.addIdentifierToCategory
    <$> genCategory
    <*> genIdentifier
