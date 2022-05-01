module Properties
  ( jsonRoundtrip,
  )
where

import qualified Data.Aeson as Aeson
import Hedgehog (Gen, PropertyT, forAll, (===))


jsonRoundtrip :: (Aeson.ToJSON a, Aeson.FromJSON a, Eq a, Show a) => Gen a -> PropertyT IO ()
jsonRoundtrip gen = do
  thing <- forAll gen
  Aeson.eitherDecode (Aeson.encode thing) === Right thing
