module MessageDb.Handlers
  ( HandleError (..),
    Handler (..),
    runHandler,
    getMessage,
    getParsedMessage,
    Handlers,
    emptyHandlers,
    listToHandlers,
    addHandler,
    removeHandler,
    handle,
    ProjectionHandler,
    projectionHandler,
    ProjectionHandlers,
    addProjectionHandler,
    projectionHandle,
    SubscriptionHandler,
    subscriptionHandler,
    SubscriptionHandlers,
    addSubscriptionHandler,
    subscriptionHandle,
  )
where

import Control.Exception (Exception)
import Control.Monad.Except (Except, MonadError (throwError), runExcept)
import Control.Monad.Reader (MonadReader (ask), ReaderT (..))
import qualified Data.Aeson as Aeson
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Semigroup
import GHC.Generics (Generic)
import MessageDb.Message (Message)
import qualified MessageDb.Message as Message


-- | An error that may occur from handling a message.
data HandleError
  = HandlerParseFailure Message.ParseMessageFailure
  | HandlerNotFound
  deriving (Show, Eq, Generic)


instance Exception HandleError
instance Aeson.ToJSON HandleError
instance Aeson.FromJSON HandleError


newtype Handler output = Handler
  { fromHandler :: ReaderT Message (Except HandleError) output
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Message
    , MonadError HandleError
    )


instance Semigroup output => Semigroup (Handler output) where
  left <> right = do
    leftOutput <- left
    rightOutput <- right
    pure $ leftOutput <> rightOutput


instance Monoid output => Monoid (Handler output) where
  mempty = pure mempty


runHandler :: Handler output -> Message -> Either HandleError output
runHandler handler message =
  runExcept $ runReaderT (fromHandler handler) message


getMessage :: Handler Message
getMessage =
  ask


getParsedMessage ::
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Handler (Message.ParsedMessage payload metadata)
getParsedMessage = do
  message <- ask

  case Message.parseMessage message of
    Left err ->
      throwError $ HandlerParseFailure err
    Right msg ->
      pure msg


type Handlers output =
  Map Message.MessageType (Handler output)


emptyHandlers :: Handlers output
emptyHandlers =
  Map.empty


listToHandlers :: [(Message.MessageType, Handler output)] -> Handlers output
listToHandlers =
  Map.fromList


addHandler :: Message.MessageType -> Handler output -> Handlers output -> Handlers output
addHandler =
  Map.insert


removeHandler :: Message.MessageType -> Handlers output -> Handlers output
removeHandler =
  Map.delete


handle :: Handlers output -> Message -> Either HandleError output
handle handlers message =
  case Map.lookup (Message.messageType message) handlers of
    Nothing ->
      Left HandlerNotFound
    Just handler ->
      runHandler handler message


type ProjectionHandler state =
  Handler (Endo state)


projectionHandler ::
  forall payload metadata state.
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> state -> state) ->
  ProjectionHandler state
projectionHandler original = do
  message <- getMessage
  Message.ParsedMessage{..} <- getParsedMessage @payload @metadata
  pure . Endo $ \state -> original message parsedPayload parsedMetadata state


type ProjectionHandlers state =
  Handlers (Endo state)


addProjectionHandler ::
  forall payload metadata state.
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Message.MessageType ->
  (Message -> payload -> metadata -> state -> state) ->
  ProjectionHandlers state ->
  ProjectionHandlers state
addProjectionHandler messageType original =
  addHandler messageType (projectionHandler original)


projectionHandle :: ProjectionHandlers state -> Message -> state -> Either HandleError state
projectionHandle handlers message state =
  let result = handle handlers message
   in fmap (($ state) . appEndo) result


type SubscriptionHandler =
  Handler (IO ())


subscriptionHandler ::
  forall payload metadata.
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  (Message -> payload -> metadata -> IO ()) ->
  SubscriptionHandler
subscriptionHandler original = do
  message <- ask
  Message.ParsedMessage{..} <- getParsedMessage @payload @metadata
  pure $ original message parsedPayload parsedMetadata


type SubscriptionHandlers =
  Handlers (IO ())


addSubscriptionHandler ::
  forall payload metadata.
  (Aeson.FromJSON payload, Aeson.FromJSON metadata) =>
  Message.MessageType ->
  (Message -> payload -> metadata -> IO ()) ->
  SubscriptionHandlers ->
  SubscriptionHandlers
addSubscriptionHandler messageType original =
  addHandler messageType (subscriptionHandler original)


subscriptionHandle :: SubscriptionHandlers -> Message -> IO (Either HandleError ())
subscriptionHandle handlers message =
  let result = handle handlers message
   in either (pure . Left) (fmap Right) result
