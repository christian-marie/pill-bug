module Shipper.ConfigParser (
    parseConfig,
) where

import Shipper.Types
import Text.ParserCombinators.Parsec hiding ((<|>), many)
import Control.Monad
import Control.Applicative
import Control.DeepSeq
import qualified Data.ByteString.Char8 as B
import Database.Redis (PortID(PortNumber))
import Data.Restricted

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
parseConfig f = parseFromFile config f >>= either (error.show) (return.force)

-- Create a file input segment, requires only 'path'
fileInputSegment :: [ExtraInfoPair] -> ConfigSegment
fileInputSegment infos = InputSegment FileInput
    { filePaths = fp
    , fExtra    = getExtras
    }
  where
    getExtras = filter ((/="paths") . fst) infos

    fp = case lookup "paths" infos of
        Just v -> case v of
            ExtraList l   -> map mustBeString l
            ExtraString p -> [p]
            _ -> error "File input wanted list or string as 'path'"
        Nothing -> error $ "File input expected 'paths' to be specified"

    mustBeString (ExtraString s) = s
    mustBeString _ = 
        error "File input expected 'paths' to be a list or string"

-- Create a debug output segment, requires no extra data
debugOutputSegment :: [ExtraInfoPair] -> ConfigSegment
debugOutputSegment _ = OutputSegment Debug {}

-- Create a ZMQ4 output segment, requires no extra data
zmq4OutputSegment :: [ExtraInfoPair] -> ConfigSegment
zmq4OutputSegment infos = OutputSegment ZMQ4Output
    { zoServers   = getServers
    , zoTimeout   = getTimeout
    , zoPublicKey = getPublicKey
    }
  where
    getPublicKey = case lookup "public_key" infos of
        Just v -> case v of
            ExtraString s -> case toRestricted $ B.pack s of
                Just r  -> r
                Nothing -> error $ "ZMQ output got an invalid 'public_key': "
                                    ++ s
            _ -> error "ZMQ output wanted 'public_key' specified as a string"
        Nothing -> error "ZMQ4 output expected 'public_key' to be specified"
    getServers = case lookup "servers" infos of
        Just v -> case v of
            ExtraString s -> [s]
            ExtraList l   -> map onlyString l
            _ -> error "ZMQ4 output wanted list or string as 'servers'"
        Nothing -> error "ZMQ4 output expected 'servers' to be specified"
      where 
        onlyString e = case e of
            ExtraString s -> s
            _ -> error "ZMQ4 output wanted a list of strings as 'servers'"

    getTimeout = case lookup "timeout" infos of
        Just v -> case v of
            ExtraString s ->
                let r = reads s :: [(Double, String)] in
                    if null r then error $ "Invalid ZMQ timeout: " ++ s
                              else round $ (fst . head) r * 1000
            _ -> error "ZMQ4 output wanted a string as 'timeout'"
        Nothing -> 1000
       

-- Create a ZMQ4 input segment, requires no extra data
zmq4InputSegment :: [ExtraInfoPair] -> ConfigSegment
zmq4InputSegment infos = InputSegment ZMQ4Input
    { ziBind = getBind
    , ziPrivateKey = getPrivateKey
    }
  where
    getPrivateKey = case lookup "private_key" infos of
        Just v -> case v of
            ExtraString s -> case toRestricted $ B.pack s of
                Just r  -> r
                Nothing -> error $ "ZMQ input got an invalid 'private_key': "
                                    ++ s
            _ -> error "ZMQ input wanted 'private_key' specified as a string"
        Nothing -> error "ZMQ4 input expected 'private_key' to be specified"

    getBind = case lookup "bind" infos of
        Just v -> case v of
            ExtraString s -> s
            _ -> error "ZMQ input expected 'bind' to be a string"
        Nothing -> error "ZMQ input expected 'bind' to be specified, " ++
            "try something like 'tcp://*:1234'"

-- Create a Redis output segment, requires no extra data
redisOutputSegment :: [ExtraInfoPair] -> ConfigSegment
redisOutputSegment infos = OutputSegment Redis
    { rHosts   = getHost
    , rPort    = getPort
    , rAuth    = getAuth
    , rKey     = getKey
    , rTimeout = getTimeout
    }
  where
    getHost = case lookup "hosts" infos of
        Just v -> case v of
            ExtraString s -> [s]
            ExtraList l   -> map onlyString l
            _ -> error "Redis output wanted list or string as 'hosts'"
        Nothing -> error "Redis output expected 'hosts' to be specified"
      where 
        onlyString e = case e of
            ExtraString s -> s
            _ -> error "Redis output wanted a list of strings as 'host'"

    getTimeout = case lookup "timeout" infos of
        Just v -> case v of
            ExtraString s ->
                let r = reads s :: [(Double, String)] in
                    if null r then error $ "Invalid redis timeout: " ++ s
                              else round $ (fst . head) r * 1000000
            _ -> error "Redis output wanted a string as 'timeout'"
        Nothing -> 1000000
                

    -- No, you cannot currently specify a different port for each host
    getPort = case lookup "port" infos of
        Just v -> case v of
            ExtraString s ->
                let r = reads s :: [(Int, String)] in
                if null r then error $ "Invalid redis output port: " ++ s
                          else PortNumber $ (fromIntegral . fst . head) r
            _   -> error "Redis output wanted 'port' as a string"
        Nothing -> defaultPort

    getAuth = case lookup "auth" infos of
        Just v -> case v of
            ExtraString s -> Just $ B.pack s
            _ -> error "Redis output wanted 'auth' as a string"
        Nothing -> Nothing

    getKey = case lookup "key" infos of
        Just v -> case v of
            ExtraString s -> B.pack s
            _ -> error "Redis output wanted 'key' as a string"
        Nothing -> error "Redis output expected 'key' to be specified"

    defaultPort = PortNumber 6379

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
                      <|> zmq4OutputSegment  <$ (try $ string "zmq4out")
                      <|> zmq4InputSegment   <$ (try $ string "zmq4in")
                      <|> redisOutputSegment <$ string "redis"

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
literalString = strip `liftM` (many $ noneOf ",\n\r'\"[]}")
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
