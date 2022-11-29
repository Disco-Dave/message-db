module Bank.Shared.Logging
  ( LoggingConfig (..)
  , withLoggingData
  , getLogEnv
  , localLogEnv
  , getKatipContext
  , localKatipContext
  , getKatipNamespace
  , localKatipNamespace
  , LoggingReader (..)
  , logDuration
  , logException
  , logException_
  , withExceptionLogging
  )
where

import Control.Exception (SomeException)
import Control.Exception.Annotated.UnliftIO (AnnotatedException (..), annotatedExceptionCallStack, checkpoint, checkpointCallStack)
import Control.Exception.Annotated.UnliftIO qualified as AnnotatedException
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Reader (MonadReader (local), asks)
import Data.Has (Has (..))
import Data.Text (Text)
import Data.UUID.V4 qualified as UUID.V4
import Bank.Shared.Exception.Annotations (loggingWasSetup)
import Bank.Shared.Exception.Annotations qualified as Annotations
import GHC.Exception (CallStack)
import GHC.Stack (HasCallStack, callStack, prettyCallStack)
import Katip qualified
import Katip.Monadic (KatipContextTState (..))
import System.Clock qualified as Clock
import System.IO (stdout)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (bracket, isAsyncException)


data LoggingConfig = LoggingConfig
  { disable :: Bool
  , environment :: Katip.Environment
  , version :: Text
  , minSeverity :: Katip.Severity
  , useBracketFormat :: Bool
  , useColor :: Bool
  }
  deriving (Show)


withLoggingData :: (Katip.LogItem context, HasCallStack) => Katip.Namespace -> context -> LoggingConfig -> (KatipContextTState -> IO a) -> IO a
withLoggingData baseNamespace context config use =
  checkpointCallStack $ do
    let makeLogEnv = do
          logEnv <- Katip.initLogEnv baseNamespace config.environment

          if config.disable
            then pure logEnv
            else do
              scribe <-
                Katip.mkHandleScribeWithFormatter
                  (if config.useBracketFormat then Katip.bracketFormat else Katip.jsonFormat)
                  (Katip.ColorLog config.useColor)
                  stdout
                  (Katip.permitItem config.minSeverity)
                  maxBound

              Katip.registerScribe "stdout" scribe Katip.defaultScribeSettings logEnv

        addCheckpoint
          | config.disable = id
          | otherwise = checkpoint loggingWasSetup

    instanceId <- UUID.V4.nextRandom

    bracket makeLogEnv Katip.closeScribes $ \logEnv ->
      addCheckpoint $
        use
          KatipContextTState
            { ltsLogEnv = logEnv
            , ltsNamespace = Katip.Namespace []
            , ltsContext =
                ( Katip.liftPayload . mconcat $
                    [ Katip.sl "version" config.version
                    , Katip.sl "instanceId" instanceId
                    ]
                )
                  <> Katip.liftPayload context
            }


getLogEnv :: (MonadReader r m, Has KatipContextTState r) => m Katip.LogEnv
getLogEnv =
  asks $ (.ltsLogEnv) . getter @KatipContextTState


localLogEnv
  :: ( MonadReader r m
     , Has KatipContextTState r
     )
  => (Katip.LogEnv -> Katip.LogEnv)
  -> m a
  -> m a
localLogEnv f =
  local . modifier $ \original ->
    let newLogEnv = f original.ltsLogEnv
     in original{ltsLogEnv = newLogEnv}


getKatipContext :: (MonadReader r m, Has KatipContextTState r) => m Katip.LogContexts
getKatipContext =
  asks $ (.ltsContext) . getter @KatipContextTState


localKatipContext
  :: ( MonadReader r m
     , Has KatipContextTState r
     )
  => (Katip.LogContexts -> Katip.LogContexts)
  -> m a
  -> m a
localKatipContext f =
  local . modifier $ \original ->
    let newLogContexts = f original.ltsContext
     in original{ltsContext = newLogContexts}


getKatipNamespace :: (MonadReader r m, Has KatipContextTState r) => m Katip.Namespace
getKatipNamespace =
  asks $ (.ltsNamespace) . getter @KatipContextTState


localKatipNamespace
  :: ( MonadReader r m
     , Has KatipContextTState r
     )
  => (Katip.Namespace -> Katip.Namespace)
  -> m a
  -> m a
localKatipNamespace f =
  local . modifier $ \original ->
    let newNamespace = f original.ltsNamespace
     in original{ltsNamespace = newNamespace}


newtype LoggingReader m r a = LoggingReader (m a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    )


instance (MonadIO m, MonadReader r m, Has KatipContextTState r) => Katip.Katip (LoggingReader m r) where
  getLogEnv = LoggingReader getLogEnv
  localLogEnv f (LoggingReader m) = LoggingReader (localLogEnv f m)


instance (MonadIO m, MonadReader r m, Has KatipContextTState r) => Katip.KatipContext (LoggingReader m r) where
  getKatipContext = LoggingReader getKatipContext
  localKatipContext f (LoggingReader m) = LoggingReader (localKatipContext f m)
  getKatipNamespace = LoggingReader getKatipNamespace
  localKatipNamespace f (LoggingReader m) = LoggingReader (localKatipNamespace f m)


logDuration
  :: ( MonadUnliftIO m
     , Katip.KatipContext m
     , HasCallStack
     )
  => Katip.Severity
  -> Text
  -> Katip.LogStr
  -> m a
  -> m a
logDuration severity contextLabel message actionToTime = do
  let startTimer = liftIO $ Clock.getTime Clock.Monotonic

      stopTimer startTime = do
        endTime <- liftIO $ Clock.getTime Clock.Monotonic

        let duration = Clock.toNanoSecs $ endTime `Clock.diffTimeSpec` startTime
         in Katip.katipAddContext (Katip.sl contextLabel duration) $
              Katip.logLocM severity message

  bracket startTimer stopTimer $ const actionToTime


logException
  :: ( Show e
     , Katip.KatipContext m
     , HasCallStack
     )
  => Katip.Severity
  -> AnnotatedException e
  -> Katip.LogStr
  -> m ()
logException severity exception message = do
  let annotatedCallStackPayload =
        foldMap (Katip.sl "annotatedCallStack" . prettyCallStack) $
          annotatedExceptionCallStack exception

      ghcCallStackPayload =
        Katip.sl "ghcCallStack" $ prettyCallStack callStack

      annotationsPayload =
        let isNotCallStack =
              not . Annotations.is @CallStack
            annotations =
              filter isNotCallStack exception.annotations
         in if null annotations
              then mempty
              else Katip.sl "annotations" (fmap show annotations)

      exceptionPayload =
        Katip.sl "exception" (show exception.exception)

      contextPayload =
        mconcat
          [ annotatedCallStackPayload
          , ghcCallStackPayload
          , annotationsPayload
          , exceptionPayload
          ]

  Katip.katipAddContext contextPayload $ do
    Katip.logLocM severity message


logException_
  :: ( Show e
     , Katip.KatipContext m
     , HasCallStack
     )
  => Katip.Severity
  -> AnnotatedException e
  -> m ()
logException_ severity exception =
  logException severity exception "Exception thrown!"


withExceptionLogging
  :: ( Katip.KatipContext m
     , MonadUnliftIO m
     , HasCallStack
     )
  => Katip.Severity
  -> m a
  -> m a
withExceptionLogging severity action = do
  result <- AnnotatedException.tryAnnotated @SomeException action

  case result of
    Right a ->
      pure a
    Left e -> do
      unless (isAsyncException e) $
        logException_ severity e

      AnnotatedException.throw e
