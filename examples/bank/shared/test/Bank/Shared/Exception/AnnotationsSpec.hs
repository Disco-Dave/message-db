module Bank.Shared.Exception.AnnotationsSpec (spec) where

import Control.Exception.Annotated (Annotation (..))
import Data.Text (Text)
import Bank.Shared.Exception.Annotations qualified as Annotations
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "cast" $ do
    it "returns nothing for invalid types" $ do
      let annotation = Annotation @Int 123
      Annotations.cast @Bool annotation `shouldBe` Nothing

    it "returns just for valid types" $ do
      let annotation = Annotation @Int 123
      Annotations.cast @Int annotation `shouldBe` Just 123

  describe "is" $ do
    it "returns false for invalid types" $ do
      let annotation = Annotation @Int 123
      Annotations.is @Bool annotation `shouldBe` False

    it "returns true for valid types" $ do
      let annotation = Annotation @Int 123
      Annotations.is @Int annotation `shouldBe` True

  describe "find" $ do
    it "returns an empty list when not types match" $ do
      let annotations =
            [ Annotation @Bool True
            , Annotation @Int 123
            , Annotation @Int 500
            , Annotation @Text "Blah blah"
            , Annotation @Int 606
            , Annotation @Text "Blah blah blah"
            , Annotation @Bool False
            ]

          foundAnnotations =
            Annotations.find @Double annotations

      foundAnnotations `shouldBe` []

    it "filters out annotations that are different types" $ do
      let annotations =
            [ Annotation @Bool True
            , Annotation @Int 123
            , Annotation @Int 500
            , Annotation @Text "Blah blah"
            , Annotation @Int 606
            , Annotation @Text "Blah blah blah"
            , Annotation @Bool False
            ]

          foundAnnotations =
            Annotations.find @Int annotations

      foundAnnotations `shouldBe` [123, 500, 606]

  describe "has" $ do
    it "returns false when type is not found" $ do
      let annotations =
            [ Annotation @Bool True
            , Annotation @Int 123
            , Annotation @Int 500
            , Annotation @Text "Blah blah"
            , Annotation @Int 606
            , Annotation @Text "Blah blah blah"
            , Annotation @Bool False
            ]

      Annotations.has @Double annotations `shouldBe` False
      Annotations.has @Annotations.LoggingWasSetup annotations `shouldBe` False
      Annotations.has @Annotations.AppMonadWasSetup annotations `shouldBe` False

    it "filters out annotations that are different types" $ do
      let annotations =
            [ Annotation @Bool True
            , Annotation @Int 123
            , Annotation @Int 500
            , Annotation @Text "Blah blah"
            , Annotation @Int 606
            , Annotation @Text "Blah blah blah"
            , Annotation @Bool False
            , Annotations.loggingWasSetup
            , Annotations.appMonadWasSetup
            ]

      Annotations.has @Text annotations `shouldBe` True
      Annotations.has @Annotations.LoggingWasSetup annotations `shouldBe` True
      Annotations.has @Annotations.AppMonadWasSetup annotations `shouldBe` True
