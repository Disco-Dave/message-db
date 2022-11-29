module Bank.Shared.Database.ConnectionUrlSpec
  ( TestConnectionUrl (..)
  , spec
  )
where

import Bank.Shared.Database.ConnectionUrl (ConnectionUrl)
import Bank.Shared.Database.ConnectionUrl qualified as ConnectionUrl
import Control.Monad (replicateM)
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable (for_)
import Data.Function (on)
import Data.Maybe (isNothing)
import Spec.PostgresTemp (withConnectionUrl)
import Test.Hspec (Spec, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck qualified as QuickCheck


newtype TestConnectionUrl = TestConnectionUrl ConnectionUrl


instance Show TestConnectionUrl where
  show (TestConnectionUrl url) =
    Char8.unpack $ ConnectionUrl.toByteString url


instance Eq TestConnectionUrl where
  TestConnectionUrl url1 == TestConnectionUrl url2 =
    on (==) ConnectionUrl.toByteString url1 url2


instance QuickCheck.Arbitrary TestConnectionUrl where
  arbitrary =
    let genByteString = do
          let alwaysOkChars =
                mconcat
                  [ ['a' .. 'z']
                  , ['A' .. 'Z']
                  , ['0' .. '9']
                  , "~@#$%^&*()_+{[\\|/><,.;'\"]}"
                  ]

          quantityOfChars <-
            QuickCheck.chooseInt (1, 300)

          chars <-
            replicateM quantityOfChars . QuickCheck.elements $ alwaysOkChars

          pure $ Char8.pack chars
     in genByteString `QuickCheck.suchThatMap` \byteString -> do
          url <- ConnectionUrl.fromByteString byteString
          pure $ TestConnectionUrl url


spec :: Spec
spec = do
  let fromByteString =
        fmap ConnectionUrl.toByteString . ConnectionUrl.fromByteString

  it "strips whitespace" $
    let input = "  postgresql://postgres@localhost:3000/discoslab?password=password   "
        expected = Just $ Char8.strip input
        actual = fromByteString input
     in actual `shouldBe` expected

  it "rejects empty inputs" $
    let inputs = ["", "  ", "\t", "\n"]
     in for_ inputs $ \input ->
          fromByteString input `shouldSatisfy` isNothing

  it "can open a url" . withConnectionUrl $ \connectionUrl ->
    ConnectionUrl.withConnection connectionUrl $ \_ ->
      pure ()

  prop "allows all valid inputs" $ \(TestConnectionUrl _) ->
    True `shouldBe` True
