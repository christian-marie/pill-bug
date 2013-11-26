module Shipper.ConfigParser (
    parseConfig,
) where

import Shipper.Types
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Monad
import Control.Applicative
import Control.DeepSeq

-- A simple parser for pill-bug configurations.
--
-- Configurations take the form of segments with many key, values pairs.
-- That is, one key and one or more values.
--
-- For example:
-- 
-- file {
-- 	paths = [/tmp/*, /var/log/*]
-- 	yay = [this works, 'i am a list']
-- 	other = "just a string"
-- 	nested = [a nested list:, [with a list,[]]]
-- 	map = {key = value}
-- }

parseConfig :: FilePath -> IO [ConfigSegment]
parseConfig f = force `liftM` cfg
  where cfg = parseFromFile config f >>= either (error . show) return

-- Create a file input segment, requires only 'path'
fileInputSegment :: [ExtraInfoPair] -> ConfigSegment
fileInputSegment info = InputSegment FileInput
    { filePaths = fp
    , fExtra    = getExtras
    }
  where
    getExtras = filter ((/="paths") . fst) info

    fp = case lookup "paths" info of
        Just v -> case v of
            ExtraList l   -> map mustBeString l
            ExtraString p -> [p]
        Nothing -> error $ "File input expected 'paths' to be specified"

    mustBeString (ExtraString s) = s
    mustBeString _ = 
        error "File input expected 'paths' to be a list or string"

-- Create a debug output segment, requires no extra data
debugOutputSegment :: [ExtraInfoPair] -> ConfigSegment
debugOutputSegment _ = OutputSegment Debug {}

-- Create a ZMQ output segment, requires no extra data
zmqOutputSegment :: [ExtraInfoPair] -> ConfigSegment
zmqOutputSegment _ = OutputSegment ZMQ {}


-- A config is zero or more segments
config :: GenParser Char st [ConfigSegment]
config = many segment <* eof

-- Segments are built here, by applying the function parsed from beginSegment
-- to the directives eaten within that segment. Simple! Sort of.
segment :: GenParser Char st ConfigSegment
segment = ap 
    (spaces *> beginSegment)
    (spaces *> directives <* endSegment <* spaces)

-- We try to see if this is a valid segment by parsing the word before a '{'
beginSegment :: GenParser Char st ( [ExtraInfoPair] -> ConfigSegment )
beginSegment = possibleSegment <* spaces <* char '{' 
  where
    -- Any valid segments need to be defined here, returning function that
    -- builds a ConfigSegment from a list of ExtraInfos
    possibleSegment =     fileInputSegment   <$ string "file" 
                      <|> debugOutputSegment <$ string "debug"
                      <|> zmqOutputSegment   <$ string "zmq"

endSegment :: GenParser Char st ()
endSegment = void $ char '}'

directives :: GenParser Char st [ExtraInfoPair]
directives = many directive

-- We turn a = [b,  c] into ("a", ["b", "c"]), wrapped inside ExtraInfo
directive :: GenParser Char st ExtraInfoPair
directive = liftA2 (,)
    (spaces *> key <* spaces <* char '=')
    (spaces *> extraInfo <* spaces)

-- A key must not include any magic characters that mean things to values or
-- segments as they are used as an anchor for values spanning multiple lines.
key :: GenParser Char st Key
key = manyTill (noneOf "\r\n}'\"") (try . lookAhead $ equals)
  where equals = spaces <* char '='


-- A key may have a list or string, that list may have lists, and that list
-- lists and so ad infinitum. Also maps may recurse.
extraInfo :: GenParser Char st ExtraInfo
extraInfo = spaces *> (extraMap <|> extraList <|> extraString) <* spaces

extraList, extraString, extraMap :: GenParser Char st ExtraInfo

-- Strings are just wrapped strings
extraString = fmap ExtraString anyString

-- A list is denoted by [, this is convenient for not backtracking.
extraList = fmap ExtraList $ 
    char '[' *> extraInfo `sepBy` separator <* char ']'

-- A map is a nested set of directives.
extraMap = fmap ExtraMap $ 
    char '{' *> directive `sepBy` separator <* char '}'

anyString :: CharParser st String
anyString = quotedString '"' <|> quotedString '\'' <|> literalString
-- Literal strings are basically barewords, the only limitation being
-- whitespace at the beginning/end of lines, double quotes and commas.
literalString :: CharParser st String
literalString = strip `liftM` (many $ noneOf ",\n\r'\"[]")
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

lstrip, rstrip, strip:: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x " \t"
                            then lstrip xs
                            else s

rstrip = reverse . lstrip . reverse
strip = lstrip . rstrip
