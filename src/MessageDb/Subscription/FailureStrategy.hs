module MessageDb.Subscription.FailureStrategy
  ( FailureReason (..)
  , FailureStrategy
  , ignoreFailures
  )
where

import Control.Exception (SomeException)
import Control.Exception.Safe (finally)
import qualified Database.PostgreSQL.Simple as Postgres
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers
import MessageDb.Message
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription.FailedMessage


data FailureReason
  = HandleFailure HandleError
  | UnknownFailure SomeException
  deriving (Show)


newtype FailureStrategy = FailureStrategy
  { logFailure :: Message -> FailureReason -> IO ()
  }


ignoreFailures :: FailureStrategy
ignoreFailures = FailureStrategy $ \_ _ ->
  pure ()


combine :: FailureStrategy -> FailureStrategy -> FailureStrategy
combine first second = FailureStrategy $ \message reason ->
  logFailure first message reason
    `finally` logFailure second message reason


instance Semigroup FailureStrategy where
  (<>) = combine


instance Monoid FailureStrategy where
  mempty = ignoreFailures


writeToCategory :: Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeToCategory withConnection = undefined
