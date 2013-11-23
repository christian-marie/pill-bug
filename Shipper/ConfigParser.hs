module Shipper.ConfigParser (
    parseConfig,
) where

import Shipper.Types
import Text.ParserCombinators.Parsec
import Data.Maybe (fromMaybe, listToMaybe)
import Control.Monad
import Control.Applicative hiding ((<|>), many)

type Key       = String
type Value     = String
type Directive = (Key, [Value])

parseConfig :: FilePath -> IO [ConfigSegment]
parseConfig f = parseFromFile config f >>= either (error . show) return

fileInputSegment :: [Directive] -> ConfigSegment
fileInputSegment ds = InputSegment FileInput
    { fTags = tags'
    , fType = tipe'
    , filePaths = fp
    }
  where
    tags' = want "tags" []
    tipe' = fromMaybe "file" . listToMaybe $ want "type" ["file"] 
    fp = need "paths"

    want k def = fromMaybe def $ lookup k ds 

    need k = case lookup k ds of
        Just v -> v
        Nothing -> error $ "File input expected key: " ++ k

config :: GenParser Char st [ConfigSegment]
config = many segment <* eof

segment :: GenParser Char st ConfigSegment
segment = (spaces *> beginSegment) <*> (directives <* endSegment <* spaces)

beginSegment :: GenParser Char st ( [Directive] -> ConfigSegment )
beginSegment = possibleSegment <* actualSpaces <* char '{' 

possibleSegment :: GenParser Char st ([Directive] -> ConfigSegment )
possibleSegment =
    fileInputSegment <$ string "file"

endSegment :: GenParser Char st ()
endSegment = void $ char '}'

directives :: GenParser Char st [Directive]
directives = many directive

directive :: GenParser Char st Directive
directive = fmap (,)
    (actualSpaces *> key <* actualSpaces <* char '=') <*>
    (actualSpaces *> values <* spaces)

-- A key must not start with '}' as it begins directives on a new line and thus
-- it is impossible to distinguish between '}' and the beginning of a valid key
-- name.
key :: GenParser Char st Key
key = manyTill (noneOf "}") (try . lookAhead $ equals)
  where equals = actualSpaces <* char '='

values :: GenParser Char st [Value]
values = value `sepBy` separator

value :: GenParser Char st Value
value = actualSpaces *> value' <* actualSpaces
  where
    value' = manyTill anyChar $ lookAhead $ try $
        actualSpaces *> choice [ separator
                               , void $ newline
                               , endSegment
                               , eof
                               ]

separator :: GenParser Char st ()
separator = void $ char ','

actualSpaces :: GenParser Char st ()
actualSpaces = skipMany $ oneOf " \t"
