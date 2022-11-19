module MessageDb.Producer.UnexpectedStreamVersion
  ( UnexpectedStreamVersion (..)
  , parseUnexpectedStreamVersion
  )
where

import Control.Exception (Exception)
import qualified Data.ByteString.Char8 as Char8
import qualified Database.PostgreSQL.Simple as Postgres


newtype UnexpectedStreamVersion = UnexpectedStreamVersion
  { unexpectedStreamVersionSqlError :: Postgres.SqlError
  }
  deriving (Show, Eq)
instance Exception UnexpectedStreamVersion


parseUnexpectedStreamVersion :: Postgres.SqlError -> Maybe UnexpectedStreamVersion
parseUnexpectedStreamVersion sqlError@Postgres.SqlError{..} =
  {- Example error of what we are looking for:

        SqlError
          { sqlState = "P0001"
          , sqlExecStatus = FatalError
          , sqlErrorMsg = "Wrong expected version: 4 (Stream: AqZHVQR4-Pn85sUkra3, Stream Version: 10)"
          , sqlErrorDetail = ""
          , sqlErrorHint = ""
          }
  -}

  let seemsLikeTheRightErrorMessages =
        "Wrong expected version:" `Char8.isPrefixOf` sqlErrorMsg

      isTheCorrectErrorState =
        sqlState == "P0001"

      isTheCorrectExecStatus =
        sqlExecStatus == Postgres.FatalError

      isProbablyTheRightError =
        seemsLikeTheRightErrorMessages
          && isTheCorrectErrorState
          && isTheCorrectExecStatus
   in if isProbablyTheRightError
        then Just $ UnexpectedStreamVersion sqlError
        else Nothing
