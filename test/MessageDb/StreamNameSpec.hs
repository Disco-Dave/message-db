module MessageDb.StreamNameSpec
  ( spec,
  )
where

import Generators.StreamName (genCategoryName, genIdentityName, genStreamName)
import Hedgehog (forAll, (===))
import MessageDb.StreamName (StreamName (..))
import qualified MessageDb.StreamName as StreamName
import Properties (jsonRoundtrip)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)


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

    it "identity is nothing when not present" . hedgehog $ do
      categoryName <- forAll genCategoryName

      let streamName = StreamName $ StreamName.fromCategoryName categoryName
       in StreamName.identity streamName === Nothing
