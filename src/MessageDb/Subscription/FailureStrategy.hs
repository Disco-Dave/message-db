-- | Strategies for dealing with message handle failures.
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


-- | Reason why the message handle failed.
data FailureReason
  = HandleFailure HandleError
  | UnknownFailure SomeException
  deriving (Show)


instance Exception FailureReason


-- | Strategy for logging failures.
newtype FailureStrategy = FailureStrategy
  { logFailure :: Message -> FailureReason -> IO ()
  }


-- | Do nothing, ignore all failures.
ignoreFailures :: FailureStrategy
ignoreFailures = FailureStrategy $ \_ _ ->
  pure ()


-- | Combine a strategy with another so that they both run for a failure.
combine :: FailureStrategy -> FailureStrategy -> FailureStrategy
combine first second = FailureStrategy $ \message reason ->
  logFailure first message reason
    `finally` logFailure second message reason


instance Semigroup FailureStrategy where
  (<>) = combine


instance Monoid FailureStrategy where
  mempty = ignoreFailures


-- | Write a failure to a category. Use @shouldKeep@ to filter out message failures you don't want to log.
writeToCategory ::
  (FailureReason -> Bool) ->
  Functions.WithConnection ->
  StreamName.CategoryName ->
  FailureStrategy
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
              (Just metadata)
              Nothing
   in FailureStrategy logFailureToCategory


-- | Only write 'UnknownFailure's to a category.
writeUnknownFailuresToCategory :: Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeUnknownFailuresToCategory =
  writeToCategory $ \case
    UnknownFailure _ -> True
    _ -> False


-- | Write either 'UnknownFailure's or 'HandleFailure's to a category.
writeAllToCategory :: Functions.WithConnection -> StreamName.CategoryName -> FailureStrategy
writeAllToCategory =
  writeToCategory $ const True
