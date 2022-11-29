module Bank.Shared.Database.IdleConnectionTimeoutSpec
  ( TestIdleConnectionTimeout (..)
  , spec
  )
where

import Bank.Shared.Database.IdleConnectionTimeout (IdleConnectionTimeout)
import Bank.Shared.Database.IdleConnectionTimeout qualified as IdleConnectionTimeout
import Data.Maybe (mapMaybe)
import Test.Hspec (Spec, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck qualified as QuickCheck


newtype TestIdleConnectionTimeout = TestIdleConnectionTimeout IdleConnectionTimeout
  deriving (Show, Eq)


instance QuickCheck.Arbitrary TestIdleConnectionTimeout where
  arbitrary =
    let generateDouble = do
          size <- do
            original <- QuickCheck.getSize
            pure $ max original 1

          int <- QuickCheck.chooseInt (5, 10 * size)
          pure $ fromIntegral int / (10 :: Double)

        toTestValue =
          fmap TestIdleConnectionTimeout . IdleConnectionTimeout.fromDouble
     in QuickCheck.suchThatMap generateDouble toTestValue


  shrink (TestIdleConnectionTimeout timeout) =
    let smallerDoubles =
          QuickCheck.shrinkRealFrac $
            IdleConnectionTimeout.toDouble timeout

        toTestValue =
          fmap TestIdleConnectionTimeout . IdleConnectionTimeout.fromDouble
     in mapMaybe toTestValue smallerDoubles


newtype TooSmall = TooSmall Double
  deriving (Show, Eq)


instance QuickCheck.Arbitrary TooSmall where
  arbitrary = do
    size <- do
      original <- QuickCheck.getSize
      pure $ max original 1

    let positive = do
          int <- QuickCheck.chooseInt (0, 4)
          pure $ fromIntegral int / (10 :: Double)

        negative = do
          int <- QuickCheck.chooseInt (0, 10 * size)
          pure $ negate (fromIntegral int / (10 :: Double))

    TooSmall <$> QuickCheck.oneof [positive, negative]


  shrink (TooSmall double) =
    [ TooSmall smaller
    | smaller <- QuickCheck.shrinkRealFrac double
    , smaller < 0.5
    ]


fromDouble :: Double -> Maybe Double
fromDouble =
  fmap IdleConnectionTimeout.toDouble . IdleConnectionTimeout.fromDouble


spec :: Spec
spec = do
  it "allows 0.5" $
    fromDouble 0.5 `shouldBe` Just 0.5

  prop "allows all numbers greater than or equal to 0.5" $ \(TestIdleConnectionTimeout _) ->
    True `shouldBe` True

  prop "rejects all numbers less than 0.5" $ \(TooSmall timeout) -> do
    fromDouble timeout `shouldBe` Nothing
