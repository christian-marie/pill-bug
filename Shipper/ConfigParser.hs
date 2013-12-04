module Shipper.ConfigParser (
    parseConfig,
) where

import Shipper.Types
import Text.Trifecta
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
parseConfig f = do
    result <- parseFromFile config f 
    return $ case result of Just cfg -> force cfg
                            Nothing -> error "Failed to parse config"

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
    { rServers = getServers
    , rAuth    = getAuth
    , rKey     = getKey
    , rTimeout = getTimeout
    }
  where
    getServers = map parseServer $ case lookup "servers" infos of
        Just v -> case v of
            ExtraString s -> [s]
            ExtraList l   -> map onlyString l
            _ -> error "Redis output wanted list or string as 'servers'"
        Nothing -> error "Redis output expected 'servers' to be specified"
      where 
        onlyString e = case e of
            ExtraString s -> s
            _ -> error "Redis output wanted a list of strings as 'host'"

        parseServer s =
            case B.split ':' $ B.pack s of 
                [host,port] -> (B.unpack host, parsePort $ B.unpack port)
                [host] -> (B.unpack host, PortNumber 6379)
                _ -> error $ "Redis output failed to parse server: " ++ s

        parsePort s =
          let r = reads s :: [(Int, String)] in
            if null r then error $ "Redis output failed to parse port: " ++ s
            else PortNumber $ (fromIntegral . fst . head) r
            

    getTimeout = case lookup "timeout" infos of
        Just v -> case v of
            ExtraString s ->
                let r = reads s :: [(Double, String)] in
                    if null r then error $ "Invalid redis timeout: " ++ s
                              else round $ (fst . head) r * 1000000
            _ -> error "Redis output wanted a string as 'timeout'"
        Nothing -> 1000000
                
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

-- A config is zero or more segments
config :: Parser [ConfigSegment]
config = many segment <* eof

-- Segments are built here, by applying the function parsed from beginSegment
-- to the directives eaten within that segment. Simple! Sort of.
segment :: Parser ConfigSegment
segment = ap 
    (spaces *> beginSegment)
    (spaces *> directives <* endSegment <* spaces)

-- We try to see if this is a valid segment by parsing the word before a '{'
beginSegment :: Parser ([ExtraInfoPair] -> ConfigSegment)
beginSegment = possibleSegment <* spaces <* char '{' 
  where
    -- Any valid segments need to be defined here, returning function that
    -- builds a ConfigSegment from a list of ExtraInfos
    possibleSegment =     fileInputSegment   <$ string "file" 
                      <|> debugOutputSegment <$ string "debug"
                      <|> zmq4OutputSegment  <$ (try $ string "zmq4out")
                      <|> zmq4InputSegment   <$ (try $ string "zmq4in")
                      <|> redisOutputSegment <$ string "redis"

endSegment :: Parser ()
endSegment = void $ char '}'

directives :: Parser [ExtraInfoPair]
directives = many directive

-- We turn a = [b,  c] into ("a", ["b", "c"]), wrapped inside ExtraInfo
directive :: Parser ExtraInfoPair
directive = liftA2 (,)
    (spaces *> key <* spaces)
    (spaces *> extraInfo <* spaces)

-- A key must not include any magic characters that mean things to values or
-- segments as they are used as an anchor for values spanning multiple lines.
key :: Parser String
key = manyTill (noneOf "\r\n}'\"") ( spaces *> char '=')


-- A key may have a list or string, that list may have lists, and that list
-- lists and so ad infinitum. Also maps may recurse.
extraInfo :: Parser ExtraInfo
extraInfo = spaces *> (extraMap <|> extraList <|> extraString) <* spaces
  where
    -- Strings are just wrapped strings
    extraString = fmap ExtraString anyString

    -- A list is denoted by [, this is convenient for not backtracking.
    extraList = fmap ExtraList $ 
        char '[' *> extraInfo `sepBy` separator <* char ']'

    -- A map is a nested set of directives.
    extraMap = fmap ExtraMap $ 
        char '{' *> directive `sepBy` separator <* char '}'

    separator = void $ char ','

anyString :: Parser String
anyString = quotedString '"' <|> quotedString '\'' <|> literalString
  where
    -- Literal strings are basically barewords, the only limitation being
    -- whitespace at the beginning/end of lines, double quotes and commas.
    literalString = strip `liftM` (many $ noneOf ",\n\r'\"[]}")
        <?> "literal string"

    -- If you need whitespace at the beginning/end of lines, double quotes or
    -- commas, you have a quoted string with \" to escape double quotes
    quotedString q = concat `liftM` do
        quote *> many quotedContents <* quote <?> "quoted string"
      where
        quote          = char q
        quotedContents = some (quotedText <|> quoteEscape)
        quotedText     = noneOf ("\\\r\n" ++ [q]) <?> "quoted text"
        quoteEscape    = char '\\' *> char q <?> "quote escape"


lstrip, rstrip, strip:: String -> String
lstrip s = case s of
                  [] -> []
                  (x:xs) -> if elem x " \t"
                            then lstrip xs
                            else s

rstrip = reverse . lstrip . reverse
strip = lstrip . rstrip
