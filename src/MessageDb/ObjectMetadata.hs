module MessageDb.ObjectMetadata
  ( ObjectMetadata (..),
    parse,
    empty,
    add,
    addList,
    remove,
    keys,
    MessageDb.ObjectMetadata.lookup,
    expand,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)


newtype ObjectMetadata = ObjectMetadata
  { fromObjectMetadata :: Aeson.Object
  }
  deriving
    ( Show
    , Eq
    , Aeson.ToJSON
    , Aeson.FromJSON
    , Semigroup
    , Monoid
    )


parse :: Aeson.FromJSON metadata => ObjectMetadata -> Maybe metadata
parse (ObjectMetadata metadata) =
  AesonTypes.parseMaybe Aeson.parseJSON $
    Aeson.Object metadata


empty :: ObjectMetadata
empty =
  mempty


add :: Aeson.ToJSON value => Text -> value -> ObjectMetadata -> ObjectMetadata
add property value (ObjectMetadata metadata) =
  ObjectMetadata $ HashMap.insert property (Aeson.toJSON value) metadata


addList :: [(Text, Aeson.Value)] -> ObjectMetadata -> ObjectMetadata
addList properties (ObjectMetadata metadata) =
  ObjectMetadata $ metadata <> HashMap.fromList properties


remove :: Text -> ObjectMetadata -> ObjectMetadata
remove property (ObjectMetadata metadata) =
  ObjectMetadata $ HashMap.delete property metadata


keys :: ObjectMetadata -> [Text]
keys (ObjectMetadata metadata) =
  HashMap.keys metadata


lookup :: Aeson.FromJSON value => Text -> ObjectMetadata -> Maybe value
lookup property (ObjectMetadata metadata) =
  HashMap.lookup property metadata
    >>= AesonTypes.parseMaybe Aeson.parseJSON


expand :: Maybe ObjectMetadata -> ObjectMetadata -> ObjectMetadata
expand original extra =
  fromMaybe mempty original <> extra
