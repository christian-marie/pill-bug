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

-- A simple parser for pill-bug configurations.
--
-- Configurations take the form of segments with many key, values pairs.
-- That is, one key and one ore more values.
--
-- For example:
-- 
-- file {
--   path = '/dev/space and a quote\'', /etc/passwd,
--          "double quotes are fine too"
--          ,also odd placement of commas and bare strings, yay
--   tags = 1,2,3
-- }
parseConfig :: FilePath -> IO [ConfigSegment]
parseConfig f = parseFromFile config f >>= either (error . show) return

-- Create a file input segment, requires only 'path'
fileInputSegment :: [Directive] -> ConfigSegment
fileInputSegment ds = InputSegment FileInput
    { fTags = tags'
    , fType = tipe'
    , filePaths = fp
    }
  where
    tags' = want "tags" []
    tipe' = fromMaybe "file" . listToMaybe $ want "type" ["file"]
    fp    = need "paths"

    want k def = fromMaybe def $ lookup k ds 

    need k = case lookup k ds of
        Just v  -> v
        Nothing -> error $ "File input expected key: " ++ k ++ show ds

-- A config is simply many unordered segments
config :: GenParser Char st [ConfigSegment]
config = some segment <* eof

-- Segments are built here, by applying the function parsed from beginSegment
-- to the directives eaten within that segment. Simple! Sort of.
segment :: GenParser Char st ConfigSegment
segment = ap 
    (spaces *> beginSegment)
    (spaces *> directives <* endSegment <* spaces)

-- We try to see if this is a valid segment by parsing the word before a '{'
beginSegment :: GenParser Char st ( [Directive] -> ConfigSegment )
beginSegment = possibleSegment <* spaces <* char '{' 
  where
    -- Any valid segments need to be defined here, returning function that
    -- builds a ConfigSegment from a list of Directives
    possibleSegment = fileInputSegment <$ string "file"

endSegment :: GenParser Char st ()
endSegment = void $ char '}'

directives :: GenParser Char st [Directive]
directives = some directive

-- We turn a = b & c into ("a", ["b", "c"])
directive :: GenParser Char st Directive
directive = liftA2 (,)
    (spaces *> key <* spaces <* char '=')
    (spaces *> values <* spaces)

-- A key must not include any magic characters that mean things to values or
-- segments as they are used as an anchor for values spanning multiple lines.
key :: GenParser Char st Key
key = manyTill (noneOf "\r\n}'\"") (try . lookAhead $ equals)
  where equals = spaces <* char '='

values :: GenParser Char st [Value]
values = value `sepBy` separator

value :: GenParser Char st Value
value = spaces *> (
        quotedString '"'
        <|> quotedString '\''
        <|> literalString
    ) <* spaces

-- Literal strings are basically barewords, the only limitation being
-- whitespace at the beginning/end of lines, double quotes and commas.
literalString :: CharParser st String
literalString = rstrip `liftM` (many $ noneOf ",\n\r'\"")
    <?> "literal string"

-- If you need whitespace at the beginning/end of lines, double quotes or
-- commas, you have a quoted string with \" to escape double quotes
quotedString :: Char -> CharParser st String
quotedString q = concat `liftM` do
    quote *> many quotedContents <* quote <?> "quoted string"
  where
    quote          = char q
    quotedContents = many1 (quotedText <|> quoteEscape)
    quotedText     = noneOf ("\\\r\n" ++ [q]) <?> "quoted text"
    quoteEscape    = char '\\' *> char q <?> "quote escape"

separator :: GenParser Char st ()
separator = void $ char ','

lstrip, rstrip:: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x " \t"
                            then lstrip xs
                            else s

rstrip = reverse . lstrip . reverse
