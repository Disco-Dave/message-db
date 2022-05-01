module MessageDb.StreamNameSpec
  ( genIdentityName,
    genCategoryName,
    genStreamName,
    spec,
  )
where

import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import MessageDb.StreamName (StreamName)
import qualified MessageDb.StreamName as StreamName
import Properties (jsonRoundtrip)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)


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
   in StreamName.category . StreamName.StreamName <$> Gen.text range validCharacters


genStreamName :: Gen StreamName
genStreamName = do
  StreamName.addIdentity
    <$> genCategoryName
    <*> genIdentityName


spec :: Spec
spec = do
  describe "StreamName" $
    it "can be serialized and deserialized from json" . hedgehog $
      jsonRoundtrip genStreamName

  describe "CategoryName" $ do
    it "can be serialized and deserialized from json" . hedgehog $
      jsonRoundtrip genCategoryName

    it "can be added to an identity name" . hedgehog $ do
      categoryName <- forAll genCategoryName
      identityName <- forAll genIdentityName

      let streamName = StreamName.addIdentity categoryName identityName
       in StreamName.category streamName === categoryName

  describe "IdentityName" $ do
    it "can be serialized and deserialized from json" . hedgehog $
      jsonRoundtrip genIdentityName

    it "can be added to a category name" . hedgehog $ do
      categoryName <- forAll genCategoryName
      identityName <- forAll genIdentityName

      let streamName = StreamName.addIdentity categoryName identityName
       in StreamName.identity streamName === Just identityName
