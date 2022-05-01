module Generators
  ( genUUID,
    genUTCTime,
    genAesonValue,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Text as Text
import qualified Data.Time as Time
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Hedgehog (Gen)
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


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
genUTCTime =
  -- TODO
  undefined


genAesonValue :: Gen Aeson.Value
genAesonValue =
  -- TODO
  undefined
