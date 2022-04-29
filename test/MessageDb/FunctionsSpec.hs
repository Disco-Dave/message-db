module MessageDb.FunctionsSpec
  ( spec,
  )
where

import Control.Monad (replicateM_)
import qualified Database.PostgreSQL.Simple as Simple
import qualified MessageDb.Functions as Functions
import TempMessageDb (withConnection)
import Test.Hspec
import UnliftIO.Exception (try)


spec :: Spec
spec =
  around withConnection $ do
    describe "writeMessage" $ do
      it "throws exception when expected version check fails" $ \connection -> do
        replicateM_ 100 $
          Functions.writeMessage
            connection
            "foobar-2000"
            "Unit"
            ()
            (Nothing :: Maybe ())
            Nothing

        _ <-
          Functions.writeMessage
            connection
            "foobar-2000"
            "Unit"
            ()
            (Nothing :: Maybe ())
            (Just 99)

        results <-
          try @_ @Simple.SqlError $
            Functions.writeMessage
              connection
              "foobar-2000"
              "Unit"
              ()
              (Nothing :: Maybe ())
              (Just 64)

        let errorMsg =
              case results of
                Right _ -> ""
                Left err -> Simple.sqlErrorMsg err

        errorMsg `shouldBe` "Wrong expected version: 64 (Stream: foobar-2000, Stream Version: 100)"
