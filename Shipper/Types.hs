module Shipper.Types (
    Event(..),
    Input(..),
    Output(..),
    ConfigSegment(..),
    ExtraInfo(..),
    ExtraInfoPair,
    Key
)
where

import qualified Data.ByteString.Char8 as B
import Data.Time
import Control.DeepSeq
import Data.MessagePack 
import Blaze.ByteString.Builder
import Data.Monoid (mconcat, mappend, Monoid)
import Data.Bits

data Event = Event
    { message :: B.ByteString
    , extra   :: [ExtraInfoPair]
    , time    :: UTCTime
    } deriving (Show)

-- It's a little tricky to get haskell objects packed into msgpack objects as
-- most datatypes are homgeneous. We get arround this by creating our own
-- Packable instance that expresses an Event as a heterogeneous map.
instance Packable Event where
    from = fromMap eventLen (mconcat . kvs)
      where
        -- A msgpack event is the message, then all of the extra data tacked on
        kvs e = [ from "message" `mappend` from (message e) ]
                ++ map extract (validExtras e)
        -- We obviously cannot attach "message" then.
        validExtras e  = filter ((/="message") . fst) (extra e)
        extract (k, v) = from k `mappend` from v
        eventLen e     = length (extra e) + 1

-- We must make this info packable in of itself as it needs to be able to
-- recurse in the event of a nested array
instance Packable ExtraInfo where
    from (ExtraString s) = from s
    from (ExtraList l)   = from l
        
-- Pulled from Data.MessagePack.
-- TODO: request that this is exposed as it is useful and we shouldn't have to
--       steal it
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

fromMap :: (a -> Int) -> (a -> Builder) -> a -> Builder
fromMap lf pf m =
  case lf m of
    len | len <= 15 ->
      fromWord8 $ 0x80 .|. fromIntegral len
    len | len < 0x10000 ->
      fromWord8 0xDE <>
      fromWord16be (fromIntegral len)
    len ->
      fromWord8 0xDF <>
      fromWord32be (fromIntegral len)
  <> pf m

-- Theses represent things like tags and type attached to a log event.
-- They will be converted to "@tags" : ["hai", "you"] and "@type" : "tipe"
data ExtraInfo = ExtraString String | ExtraList [ExtraInfo]
    deriving (Show)

type ExtraInfoPair = (Key, ExtraInfo)
type Key           = String

data Input = FileInput
    { fExtra    :: [ExtraInfoPair]
    , filePaths :: [String] -- May be globs
    } deriving (Show)

data Output = Debug | ZMQ
    deriving (Show)

data ConfigSegment = InputSegment Input | OutputSegment Output
    deriving (Show)

instance NFData ConfigSegment where
     rnf (InputSegment a)  = a `deepseq` ()
     rnf (OutputSegment a) = a `deepseq` ()

instance NFData Input where
     rnf (FileInput a b) = a `deepseq` b `deepseq` ()

instance NFData ExtraInfo where
     rnf (ExtraString a) = a `deepseq` ()
     rnf (ExtraList a)   = a `deepseq` ()

instance NFData Output where
     rnf Debug = ()
     rnf ZMQ = ()
