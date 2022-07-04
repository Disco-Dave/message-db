{- FOURMOLU_DISABLE -}

{-# LANGUAGE CPP #-}

module Generators
  ( genUUID
  , genUTCTime
  , genAesonValue
  )
where

import qualified Data.Aeson as Aeson
import Data.Bifunctor (second)
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.Vector as Vector
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Numeric.Natural (Natural)

#if MIN_VERSION_aeson(2, 0, 0)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
#else
import qualified Data.HashMap.Strict as HashMap
#endif


genUUID :: Gen UUID
genUUID = do
  segments <-
    let digit n =
          let range = Range.constant n n
           in Gen.text range Gen.hexit
     in sequenceA
          [ digit 8
          , digit 4
          , digit 4
          , digit 4
          , digit 12
          ]

  case UUID.fromText (Text.intercalate "-" segments) of
    Nothing -> fail "Invalid UUID was generated, please report this as a bug."
    Just uuid -> pure uuid


genUTCTime :: Gen Time.UTCTime
genUTCTime = do
  year <- Gen.integral (Range.linear 2010 2030)
  month <- Gen.integral (Range.linear 1 12)
  day <- Gen.integral (Range.linear 1 31)

  secondsFromMidnight <- Time.secondsToDiffTime <$> Gen.integral (Range.linear 0 86_401)

  pure $ Time.UTCTime (Time.fromGregorian year month day) secondsFromMidnight


genAesonValue :: Gen Aeson.Value
genAesonValue = 
  let genNull =
        pure Aeson.Null

      genBool =
        fmap Aeson.Bool Gen.bool

      genNumber =
        Aeson.Number <$> Gen.realFrac_ (Range.constantFrom 0 (-10_000) 10_000)

      genString =
        Aeson.String <$> Gen.text (Range.linear 0 100) Gen.alpha

      choices depth =
        [ (1, genNull)
        , (1, genBool)
        , (1, genNumber)
        , (1, genString)
        , (if depth > 0 then 1 else 0, genArray depth)
        , (if depth > 0 then 1 else 0, genObject depth)
        ]

      genArray factor =
        let genList = Gen.list (Range.linear 0 10)
            elements = fmap (second genList) (choices (factor - 1))
         in Aeson.Array . Vector.fromList <$> Gen.frequency elements

#if MIN_VERSION_aeson(2, 0, 0)
      genObject factor =
        let genPropertyName =
              Key.fromText <$> Gen.text (Range.linear 1 10) Gen.alpha

            genPropertyValue =
              Gen.frequency (choices (factor - 1))

            property =
              (,) <$> genPropertyName <*> genPropertyValue

            pairs =
              Gen.list (Range.linear 0 10) property
         in fmap (Aeson.Object . KeyMap.fromList) pairs
#else
      genObject factor =
        let genPropertyName =
              Gen.text (Range.linear 1 10) Gen.alpha

            genPropertyValue =
              Gen.frequency (choices (factor - 1))

            property =
              (,) <$> genPropertyName <*> genPropertyValue

            pairs =
              Gen.list (Range.linear 0 10) property
         in fmap (Aeson.Object . HashMap.fromList) pairs
#endif
   in 
    let startingDepth = 3 :: Natural
     in Gen.frequency 
        [ (1, genNull)
        , (1, genBool)
        , (1, genNumber)
        , (1, genString)
        , (2, genArray startingDepth)
        , (5, genObject startingDepth)
        ]
