module MessageDb.Consumer.ConsumerGroup
  ( ConsumerGroup (..)
  , singleConsumer
  , isSingleConsumer
  )
where

import Data.Aeson ((.:), (.=))
import qualified Data.Aeson as Aeson
import MessageDb.Consumer.ConsumerGroupSize (ConsumerGroupSize (..))
import MessageDb.Consumer.ConsumerIndex (ConsumerIndex (..))


data ConsumerGroup = ConsumerGroup
  { consumerIndex :: ConsumerIndex
  , consumerGroupSize :: ConsumerGroupSize
  }
  deriving (Show, Eq)


singleConsumer :: ConsumerGroup
singleConsumer = ConsumerGroup 0 1


isSingleConsumer :: ConsumerGroup -> Bool
isSingleConsumer =
  (== singleConsumer)


toKeyValues :: Aeson.KeyValue kv => ConsumerGroup -> [kv]
toKeyValues ConsumerGroup{..} =
  [ "index" .= consumerIndex
  , "groupSize" .= consumerGroupSize
  ]


instance Aeson.ToJSON ConsumerGroup where
  toJSON = Aeson.object . toKeyValues
  toEncoding = Aeson.pairs . mconcat . toKeyValues


instance Aeson.FromJSON ConsumerGroup where
  parseJSON = Aeson.withObject "ConsumerGroup" $ \object -> do
    consumerIndex <- object .: "index"
    consumerGroupSize <- object .: "groupSize"
    pure $ ConsumerGroup{..}
