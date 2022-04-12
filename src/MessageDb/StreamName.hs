-- | Captures the rules and structure described on the eventide website here: http://docs.eventide-project.org/core-concepts/streams/stream-names.html#entity-stream-name
module MessageDb.StreamName (
  -- * Stream Segment
  StreamSegment,
  segmentFromText,
  segmentToText,

  -- * Stream Name
  StreamName (..),
  toText,
  fromText,
) where

import Control.Monad (when)
import Data.Bifunctor (first)
import Data.List (intercalate, nub, (\\))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Error as ParsecError
import Text.Parsec.Text (Parser)

newtype StreamSegment = StreamSegment Text
  deriving
    ( Show
    , Eq
    , Ord
    )

segmentToText :: StreamSegment -> Text
segmentToText (StreamSegment text) =
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

segmentFromText :: Text -> Maybe StreamSegment
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
        else Just (StreamSegment stripped)

data StreamName = StreamName
  { category :: StreamSegment
  , command :: Maybe StreamSegment
  , entity :: Maybe StreamSegment
  , position :: Maybe StreamSegment
  }
  deriving (Show, Eq, Ord)

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

segmentParser :: Parser StreamSegment
segmentParser = do
  characters <- Parsec.many1 (Parsec.satisfy (not . (`elem` separators)))
  pure . StreamSegment $ Text.pack characters

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
         in fail $ "Duplicate stream segment(s) detected: " <> separatorCsv

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
