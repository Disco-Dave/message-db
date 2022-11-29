module Bank.Shared.EnvParsersSpec (spec) where

import Bank.Shared.EnvParsers qualified as EnvParsers
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text qualified as Text
import Env qualified
import Test.Hspec (Spec, describe, it, shouldBe)


spec :: Spec
spec = do
  describe "parseString" $ do
    it "rejects empty strings" $
      let inputs =
            [ ""
            ]

          expected = Left Env.empty
       in for_ inputs $ \input ->
            EnvParsers.parseString @Text Right input `shouldBe` expected

    it "rejects when parsing function fails" $
      let inputs =
            [ ("f", "first error")
            , ("foobar", "second error")
            , (" abc ", "third error")
            ]
       in for_ inputs $ \(input, errorMessage) ->
            let actual = EnvParsers.parseString @Text @Text (const $ Left errorMessage) input
                expected = Left (Env.UnreadError (Text.unpack errorMessage))
             in actual `shouldBe` expected

    it "accepts non-empty strings that pass parsing function" $
      let inputs =
            [ "f"
            , "foobar"
            , " abc "
            ]
       in for_ inputs $ \input ->
            let actual = EnvParsers.parseString @Text @Text Right input
                expected = Right (Text.pack input)
             in actual `shouldBe` expected
