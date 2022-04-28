module MessageDb.FunctionsSpec
  ( spec,
  )
where

import qualified MessageDb.Functions as Functions
import TempMessageDb (withConnection)
import Test.Hspec


spec :: Spec
spec =
  around withConnection $ do
    it "it works!!!" $ \connection -> do
      messages <-
        Functions.getStreamMessages
          connection
          "foobar-123"
          Nothing
          Nothing
          Nothing

      messages `shouldBe` []
