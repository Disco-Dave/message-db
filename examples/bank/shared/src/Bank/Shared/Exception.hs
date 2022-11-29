module Bank.Shared.Exception
  ( finalExceptionHandler
  )
where

import Control.Exception (SomeException, throwIO)
import Control.Exception.Annotated.UnliftIO (AnnotatedException (..), tryAnnotated)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Bank.Shared.Exception.Annotations qualified as Annotations
import System.Exit (exitFailure)
import System.IO (hPrint, stderr)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (isAsyncException)


finalExceptionHandler :: MonadUnliftIO m => m a -> m a
finalExceptionHandler action = do
  result <- tryAnnotated @SomeException action

  liftIO $ case result of
    Right a -> pure a
    Left e -> do
      when (isAsyncException e) $ throwIO e.exception

      let loggingWasSetup =
            Annotations.has @Annotations.LoggingWasSetup e.annotations

          appMonadWasSetup =
            Annotations.has @Annotations.AppMonadWasSetup e.annotations

      unless (loggingWasSetup && appMonadWasSetup) $
        hPrint stderr e.exception

      exitFailure
