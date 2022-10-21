module MessageDb.Subscription.FailureStrategy
  ( FailureReason (..)
  , FailureStrategy (..)
  , ignoreFailures
  , writeToCategory
  , writeUnknownFailuresToCategory
  , writeAllToCategory
  )
where

import Control.Exception.Safe (finally)
import Control.Monad (void, when)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID.V4
import qualified MessageDb.Functions as Functions
import qualified MessageDb.Message as Message
import qualified MessageDb.StreamName as StreamName
import MessageDb.Subscription.FailedMessage (FailedMessage (FailedMessage), FailureReason)
import qualified MessageDb.Subscription.FailedMessage as FailedMessage


-- | Strategy for logging failures.
newtype FailureStrategy = FailureStrategy
  { logFailure :: FailedMessage -> IO ()
  }


-- | Do nothing, ignore all failures.
ignoreFailures :: FailureStrategy
ignoreFailures = FailureStrategy $ \_ ->
  pure ()


-- | Combine a strategy with another so that they both run for a failure.
combine :: FailureStrategy -> FailureStrategy -> FailureStrategy
combine first second = FailureStrategy $ \message ->
  logFailure first message `finally` logFailure second message


instance Semigroup FailureStrategy where
  (<>) = combine


instance Monoid FailureStrategy where
  mempty = ignoreFailures


-- | Write a failure to a category. Use @shouldKeep@ to filter out message failures you don't want to log.
writeToCategory
  :: (FailureReason -> Bool)
  -> Functions.WithConnection
  -> StreamName.Category
  -> FailureStrategy
writeToCategory shouldKeep withConnection categoryName =
  let logFailureToCategory payload@FailedMessage{..} =
        when (shouldKeep failedReason) $ do
          identity <-
            case StreamName.identifierOfStream (Message.messageStream failedMessage) of
              Nothing -> fmap (StreamName.Identifier . UUID.toText) UUID.V4.nextRandom
              Just value -> pure value

          let streamName =
                StreamName.addIdentifierToCategory categoryName identity

              metadata = Message.messageMetadata failedMessage

          void . withConnection $ \connection ->
            Functions.writeMessage
              connection
              streamName
              (Message.messageTypeOf @FailedMessage)
              payload
              (Just metadata)
              Nothing
   in FailureStrategy logFailureToCategory


-- | Only write 'UnknownFailure's to a category.
writeUnknownFailuresToCategory :: Functions.WithConnection -> StreamName.Category -> FailureStrategy
writeUnknownFailuresToCategory =
  writeToCategory $ \case
    FailedMessage.UnknownFailure _ -> True
    _ -> False


-- | Write either 'UnknownFailure's or 'HandleFailure's to a category.
writeAllToCategory :: Functions.WithConnection -> StreamName.Category -> FailureStrategy
writeAllToCategory =
  writeToCategory $ const True
