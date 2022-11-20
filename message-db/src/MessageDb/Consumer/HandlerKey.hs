module MessageDb.Consumer.HandlerKey
  ( HandlerKey (..)
  , lookupByMessageType
  )
where

import Control.Applicative (Alternative ((<|>)))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import MessageDb.Message.MessageType (MessageType)


data HandlerKey
  = ByMessageType MessageType
  | AnyMessageType
  deriving (Show, Eq, Ord)


lookupByMessageType :: MessageType -> Map HandlerKey a -> Maybe a
lookupByMessageType messageType handlers =
  Map.lookup (ByMessageType messageType) handlers
    <|> Map.lookup AnyMessageType handlers
