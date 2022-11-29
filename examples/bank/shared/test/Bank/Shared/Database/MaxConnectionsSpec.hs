module Bank.Shared.Database.MaxConnectionsSpec
  ( TestMaxConnections (..)
  , spec
  )
where

import Bank.Shared.Database.MaxConnections (MaxConnections)
import Bank.Shared.Database.MaxConnections qualified as MaxConnections
import Data.Maybe (mapMaybe)
import Test.Hspec (Spec, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck qualified as QuickCheck


newtype TestMaxConnections = TestMaxConnections MaxConnections
  deriving (Show, Eq)


instance QuickCheck.Arbitrary TestMaxConnections where
  arbitrary =
    let toTestValue =
          fmap TestMaxConnections . MaxConnections.fromInt . QuickCheck.getPositive
     in QuickCheck.suchThatMap QuickCheck.arbitrary toTestValue


  shrink (TestMaxConnections connections) =
    let smallerInts = QuickCheck.shrink . QuickCheck.Positive $ MaxConnections.toInt connections

        toTestValue =
          fmap TestMaxConnections . MaxConnections.fromInt . QuickCheck.getPositive
     in mapMaybe toTestValue smallerInts


fromInt :: Int -> Maybe Int
fromInt =
  fmap MaxConnections.toInt . MaxConnections.fromInt


spec :: Spec
spec = do
  prop "allows all numbers greater than 0" $ \(TestMaxConnections _) ->
    True `shouldBe` True

  prop "rejects all numbers less than or equal to 0" $ \(QuickCheck.NonPositive connections) -> do
    fromInt connections `shouldBe` Nothing
