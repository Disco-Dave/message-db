module MessageDb.Subscription.FailureStrategy
  ( FailureReason (..),
    FailureStrategy (..),
    ignoreFailures,
    writeToCategory,
    writeUnknownFailuresToCategory,
    writeAllToCategory,
  )
where

import Control.Exception (Exception, SomeException)
import Control.Exception.Safe (finally)
import Control.Monad (void, when)
import qualified Data.Text as Text
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified MessageDb.Functions as Functions
import MessageDb.Handlers
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription.FailedMessage (FailedMessage (FailedMessage))
import qualified MessageDb.Subscription.FailedMessage as FailedMessage


data FailureReason
  = HandleFailure HandleError
  | UnknownFailure SomeException
  deriving (Show)
instance Exception FailureReason


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


writeToCategory :: (FailureReason -> Bool) -> Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeToCategory shouldKeep withConnection categoryName =
  let logFailureToCategory message reason =
        when (shouldKeep reason) $ do
          identity <-
            case StreamName.identity (Message.streamName message) of
              Nothing -> fmap (StreamName.IdentityName . UUID.toText) UUID.V4.nextRandom
              Just value -> pure value

          let streamName =
                StreamName.addIdentity categoryName identity

              payload =
                FailedMessage
                  { message = message
                  , reason = Text.pack $ show reason
                  }

              metadata = Message.metadata message

          void . withConnection $ \connection ->
            Functions.writeMessage
              connection
              streamName
              FailedMessage.messageType
              payload
              metadata
              Nothing
   in FailureStrategy logFailureToCategory


writeUnknownFailuresToCategory :: Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeUnknownFailuresToCategory =
  writeToCategory $ \case
    UnknownFailure _ -> True
    _ -> False


writeAllToCategory :: Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeAllToCategory =
  writeToCategory $ const True
