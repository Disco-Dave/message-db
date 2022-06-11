module MessageDb.StreamNameSpec
  ( spec,
  )
where

import Generators.StreamName (genCategory, genIdentifier, genStreamName)
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

  describe "Category" $ do
    it "can be serialized and deserialized from json" . hedgehog $
      jsonRoundtrip genCategory

    it "can be added to an identifier" . hedgehog $ do
      categoryName <- forAll genCategory
      identityName <- forAll genIdentifier

      let streamName = StreamName.addIdentifierToCategory categoryName identityName
       in StreamName.categoryOfStream streamName === categoryName

  describe "Identifier" $ do
    it "can be serialized and deserialized from json" . hedgehog $
      jsonRoundtrip genIdentifier

    it "can be added to a category name" . hedgehog $ do
      categoryName <- forAll genCategory
      identityName <- forAll genIdentifier

      let streamName = StreamName.addIdentifierToCategory categoryName identityName
       in StreamName.identifierOfStream streamName === Just identityName

    it "identifier is nothing when not present" . hedgehog $ do
      categoryName <- forAll genCategory

      let streamName = StreamName $ StreamName.categoryToText categoryName
       in StreamName.identifierOfStream streamName === Nothing
