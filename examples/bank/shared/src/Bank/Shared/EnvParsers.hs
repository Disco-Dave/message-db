module Bank.Shared.EnvParsers
  ( parseString
  , parseAuto
  , bool
  , connectionUrl
  , maxConnections
  , idleConnectionTimeout
  , katipEnvironment
  , katipSeverity
  , loggingConfig
  , databaseConfig
  )
where

import Bank.Shared.Database (DatabaseConfig (..))
import Bank.Shared.Database.ConnectionUrl (ConnectionUrl)
import Bank.Shared.Database.ConnectionUrl qualified as ConnectionUrl
import Bank.Shared.Database.IdleConnectionTimeout (IdleConnectionTimeout)
import Bank.Shared.Database.IdleConnectionTimeout qualified as IdleConnectionTimeout
import Bank.Shared.Database.MaxConnections (MaxConnections)
import Bank.Shared.Database.MaxConnections qualified as MaxConnections
import Bank.Shared.Logging (LoggingConfig (..))
import Control.Monad ((<=<))
import Control.Monad.Reader (ReaderT (..))
import Control.Monad.Trans (MonadTrans (lift))
import Data.Bifunctor (Bifunctor (first))
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as Text
import Env qualified
import Katip qualified


parseString :: IsString str => (str -> Either Text a) -> Env.Reader Env.Error a
parseString fromString = runReaderT $ do
  string <- ReaderT Env.nonempty
  lift . first (Env.UnreadError . Text.unpack) $ fromString string


parseAuto :: Read i => (i -> Either Text a) -> Env.Reader Env.Error a
parseAuto fromInput = runReaderT $ do
  i <- ReaderT Env.auto
  lift . first (Env.UnreadError . Text.unpack) $ fromInput i


bool :: Env.Reader Env.Error Bool
bool =
  parseString $ \input ->
    let rawText =
          Text.toCaseFold $ Text.strip input

        possibleValues =
          [ (Text.toCaseFold "true", True)
          , (Text.toCaseFold "t", True)
          , (Text.toCaseFold "1", True)
          , (Text.toCaseFold "yes", True)
          , (Text.toCaseFold "y", True)
          , (Text.toCaseFold "false", False)
          , (Text.toCaseFold "f", False)
          , (Text.toCaseFold "0", False)
          , (Text.toCaseFold "no", False)
          , (Text.toCaseFold "n", False)
          ]
     in case lookup rawText possibleValues of
          Nothing ->
            Left . mconcat $
              [ rawText
              , " is not a valid value. Valid values are true, false, t, f, yes, no, y, n, 1, or 0."
              ]
          Just value -> Right value


connectionUrl :: Env.Reader Env.Error ConnectionUrl
connectionUrl =
  parseString $ \str ->
    case ConnectionUrl.fromByteString str of
      Nothing -> Left "Connection URL may not be empty."
      Just value -> pure value


maxConnections :: Env.Reader Env.Error MaxConnections
maxConnections =
  parseAuto $ \i ->
    case MaxConnections.fromInt i of
      Nothing -> Left "Must be greater than 0"
      Just value -> Right value


idleConnectionTimeout :: Env.Reader Env.Error IdleConnectionTimeout
idleConnectionTimeout =
  parseAuto $ \i ->
    case IdleConnectionTimeout.fromDouble i of
      Nothing -> Left "Must be greater than or equal to 0.5"
      Just value -> Right value


katipSeverity :: Env.Reader Env.Error Katip.Severity
katipSeverity =
  parseString $ \input ->
    let rawText =
          Text.toCaseFold $ Text.strip input

        possibleValues =
          [ (Text.toCaseFold "debug", Katip.DebugS)
          , (Text.toCaseFold "info", Katip.InfoS)
          , (Text.toCaseFold "notice", Katip.NoticeS)
          , (Text.toCaseFold "warning", Katip.WarningS)
          , (Text.toCaseFold "error", Katip.ErrorS)
          , (Text.toCaseFold "critical", Katip.CriticalS)
          , (Text.toCaseFold "alert", Katip.AlertS)
          , (Text.toCaseFold "emergency", Katip.EmergencyS)
          ]

        possibleValueKeys =
          Text.intercalate ", " $ fmap fst possibleValues
     in case lookup rawText possibleValues of
          Nothing ->
            Left . mconcat $
              [ rawText
              , " is not a valid value. Valid values are "
              , possibleValueKeys
              ]
          Just value -> Right value


katipEnvironment :: Env.Reader Env.Error Katip.Environment
katipEnvironment =
  parseString (Right . Katip.Environment)


loggingConfig :: Env.Parser Env.Error LoggingConfig
loggingConfig =
  Env.prefixed "LOGGING_" $
    LoggingConfig
      <$> Env.var bool "DISABLE" (Env.help "Disable all logging. Defaults to false." <> Env.def False)
      <*> Env.var katipEnvironment "ENVIRONMENT" (Env.help "Name describing the environment the app is running in. Usually something like local, dev, qa, or prod.")
      <*> Env.var (Env.str <=< Env.nonempty) "VERSION" (Env.help "Version of the application. Usually a semver or a git sha.")
      <*> Env.var katipSeverity "MIN_SEVERITY" (Env.help "Minimum severity to emit a log for. Default is debug." <> Env.def minBound)
      <*> Env.var bool "USE_BRACKET_FORMAT" (Env.help "Use the bracket format instead of the json format. Defaults to false." <> Env.def False)
      <*> Env.var bool "USE_COLOR" (Env.help "Use color for logs that are warning or higher. Defaults to false." <> Env.def False)


databaseConfig :: Env.Parser Env.Error DatabaseConfig
databaseConfig =
  DatabaseConfig
    <$> Env.var connectionUrl "URL" (Env.help "Connection URL for the database managed by the discoslab-database project.")
    <*> Env.var idleConnectionTimeout "IDLE_CONNECTION_TIMEOUT" (Env.help "How long to keep an idle connection open for. Defaults to 15 seconds." <> Env.def IdleConnectionTimeout.def)
    <*> Env.var maxConnections "MAX_CONNECTIONS" (Env.help "Max number of connections to allow open in the pool. Defaults to 5." <> Env.def MaxConnections.def)
