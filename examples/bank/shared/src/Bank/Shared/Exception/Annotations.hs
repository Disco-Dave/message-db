module Bank.Shared.Exception.Annotations
  ( cast
  , is
  , find
  , has
  , LoggingWasSetup (..)
  , loggingWasSetup
  , AppMonadWasSetup (..)
  , appMonadWasSetup
  )
where

import Control.Exception.Annotated (Annotation (..))
import Data.Kind (Type)
import Data.Maybe (isJust, mapMaybe)
import Data.Typeable (Typeable)
import Data.Typeable qualified as Typeable


cast :: forall (target :: Type). Typeable target => Annotation -> Maybe target
cast (Annotation annotation) =
  Typeable.cast annotation


is :: forall (target :: Type). Typeable target => Annotation -> Bool
is =
  isJust . cast @target


find :: forall (target :: Type). Typeable target => [Annotation] -> [target]
find =
  mapMaybe cast


has :: forall (target :: Type). Typeable target => [Annotation] -> Bool
has =
  any (is @target)


data LoggingWasSetup = LoggingWasSetup
  deriving (Show)


loggingWasSetup :: Annotation
loggingWasSetup =
  Annotation LoggingWasSetup


data AppMonadWasSetup = AppMonadWasSetup
  deriving (Show)


appMonadWasSetup :: Annotation
appMonadWasSetup =
  Annotation AppMonadWasSetup
