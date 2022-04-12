-- | Captures the rules and structure described on the eventide website here: http://docs.eventide-project.org/core-concepts/streams/stream-names.html#entity-stream-name
module MessageDb.StreamName (
  -- * Stream Segment
  StreamNameSegment,
  segmentFromText,
  segmentToText,

  -- * Stream Name
  StreamName (..),
  toText,
  fromText,
) where

import Control.Monad (when)
import qualified Data.Aeson as Aeson
import Data.Bifunctor (first)
import Data.Function (on)
import Data.List (intercalate, nub, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import Database.PostgreSQL.Simple.FromField (FromField)
import qualified Database.PostgreSQL.Simple.FromField as FromField
import Database.PostgreSQL.Simple.ToField (ToField)
import qualified Database.PostgreSQL.Simple.ToField as ToField
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParsecError
import Text.Parsec.Text (Parser)

newtype StreamNameSegment = StreamNameSegment Text
  deriving (Show, Eq, Ord)

segmentToText :: StreamNameSegment -> Text
segmentToText (StreamNameSegment text) =
  text

entitySeparator :: Char
entitySeparator = '-'

commandSeparator :: Char
commandSeparator = ':'

positionSeparator :: Char
positionSeparator = '+'

separators :: String
separators =
  [ entitySeparator
  , commandSeparator
  , positionSeparator
  ]

segmentFromText :: Text -> Maybe StreamNameSegment
segmentFromText text =
  let stripped = Text.strip text
      errorChecks =
        let isEmpty =
              Text.null stripped
            containsInvalidCharacters =
              Text.any (`elem` separators) stripped
         in [isEmpty, containsInvalidCharacters]
   in if or errorChecks
        then Nothing
        else Just (StreamNameSegment stripped)

instance Aeson.ToJSON StreamNameSegment where
  toJSON = Aeson.toJSON . segmentToText
  toEncoding = Aeson.toEncoding . segmentToText

instance Aeson.FromJSON StreamNameSegment where
  parseJSON = Aeson.withText "StreamNameSegment" $ \text ->
    case segmentFromText text of
      Just segment -> pure segment
      Nothing -> fail "Malformed stream name segment"

instance ToField StreamNameSegment where
  toField = ToField.toField . segmentToText

instance FromField StreamNameSegment where
  fromField field value = do
    text <- FromField.fromField field value
    case segmentFromText text of
      Just segment -> pure segment
      Nothing -> FromField.returnError FromField.ConversionFailed field "Malformed stream name segment"

data StreamName = StreamName
  { category :: StreamNameSegment
  , command :: Maybe StreamNameSegment
  , entity :: Maybe StreamNameSegment
  , position :: Maybe StreamNameSegment
  }
  deriving (Show, Eq)

toText :: StreamName -> Text
toText StreamName{..} =
  let optional separator value =
        maybe "" (Text.cons separator . segmentToText) value
   in mconcat
        [ segmentToText category
        , optional commandSeparator command
        , optional entitySeparator entity
        , optional positionSeparator position
        ]

segmentParser :: Parser StreamNameSegment
segmentParser = do
  characters <- Parsec.many1 (Parsec.satisfy (not . (`elem` separators)))
  pure . StreamNameSegment $ Text.pack characters

separatorParser :: String -> Parser Char
separatorParser selectedSeperators =
  Parsec.satisfy (`elem` selectedSeperators)

streamNameParser :: Parser StreamName
streamNameParser = do
  category <- segmentParser

  otherSegments <- Parsec.many $ do
    segmentType <- separatorParser separators
    segmentValue <- segmentParser
    pure (segmentType, segmentValue)

  let originalSegments = fmap fst otherSegments
      uniqueSegments = nub originalSegments
   in when (length originalSegments /= length uniqueSegments) $
        let duplicatedSeparators = nub $ originalSegments \\ uniqueSegments
            separatorCsv = intercalate ", " (fmap pure duplicatedSeparators)
         in fail $ "Duplicate stream name segment(s) detected: " <> separatorCsv

  let select separator =
        lookup separator otherSegments

  pure
    StreamName
      { category = category
      , command = select commandSeparator
      , entity = select entitySeparator
      , position = select positionSeparator
      }

fromText :: Text -> Either Text StreamName
fromText =
  let parseErrorToText parseError =
        let messages = ParsecError.errorMessages parseError
         in mconcat $ fmap (Text.pack . ParsecError.messageString) messages
   in first parseErrorToText . Parsec.parse streamNameParser "" . Text.strip

instance Ord StreamName where
  compare = on compare toText

instance Aeson.ToJSON StreamName where
  toJSON = Aeson.toJSON . toText
  toEncoding = Aeson.toEncoding . toText

instance Aeson.FromJSON StreamName where
  parseJSON = Aeson.withText "StreamName" $ \text ->
    case fromText text of
      Right name -> pure name
      Left err -> fail $ show err

instance ToField StreamName where
  toField = ToField.toField . toText

instance FromField StreamName where
  fromField field value = do
    text <- FromField.fromField field value
    case fromText text of
      Right name -> pure name
      Left err -> FromField.returnError FromField.ConversionFailed field (show err)
