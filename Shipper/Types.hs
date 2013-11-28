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

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Control.DeepSeq
import Data.MessagePack 
import Blaze.ByteString.Builder
import Data.Monoid (mconcat, mappend, Monoid)
import Data.Bits
import Database.Redis (HostName, PortID)
import System.ZMQ4 (Timeout)
import Data.Monoid ((<>))

data Event =
    UnpackedEvent
    { message :: B.ByteString
    , extra   :: [ExtraInfoPair]
    , time    :: String
    } 
    | PackedEvent LB.ByteString
    deriving (Show)


-- It's a little tricky to get haskell objects packed into msgpack objects as
-- most datatypes are homgeneous. We get arround this by creating our own
-- Packable instance that expresses an Event as a heterogeneous map.

-- For that, we need to be able to extract our own k, v pair
extractKV :: (Packable k, Packable v) => (k, v) -> Builder 
extractKV (k, v) = from k `mappend` from v

instance Packable Event where
    -- A packed event simply returns the packed event, this so that we can pass
    -- around already packed events that don't need to be re-packed.
    from (PackedEvent s) = fromLazyByteString s
    -- Pack up an unpacked event by taking all of the 'root' key, value pairs
    -- and turning them into a map. Nested stuff follows recursively via the
    -- ExtraInfo Packable instance below.
    from event@(UnpackedEvent _ _ _) = fromMap eventLen (mconcat . allKV) event
      where
        -- A msgpack event is the message, then all of the extra data tacked on
        --
        -- TODO: Date as a float
        --
        allKV e = 
            [ from "message" `mappend` from (message e)
            , from "@timestamp" `mappend` from (time e)
            , from "@version" `mappend` from "1" ]
                ++ map extractKV (validExtras e)
        -- We obviously cannot attach "message" then.
        validExtras e  = filter ((/="message") . fst) (extra e)
        -- The overall length is thesefore, all of the root elements + 3
        -- (message, date and version)
        eventLen      = (+3).length.extra

-- We must make this info packable in of itself as it needs to be able to
-- recurse in the event of a nested array or map
instance Packable ExtraInfo where
    from (ExtraString s) = from s
    from (ExtraList l)   = from l
    from (ExtraMap kvs) = fromMap length (mconcat . map extractKV) kvs 

-- Pulled from Data.MessagePack.
-- TODO: request that this is exposed as it is useful and we shouldn't have to
--       steal it

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
data ExtraInfo =
      ExtraString String 
    | ExtraList [ExtraInfo] 
    | ExtraMap [(Key, ExtraInfo)]
    deriving (Show)

type ExtraInfoPair = (Key, ExtraInfo)
type Key           = String

data Input = FileInput
    { fExtra    :: [ExtraInfoPair]
    , filePaths :: [String] -- May be globs
    }
    | ZMQ4Input
    { ziBind :: String }
    deriving (Show)

data Output =
      Debug
    | ZMQ4Output
    { zoServers :: [String]
    , zoTimeout :: Timeout
    }
    | Redis
    { rHosts   :: [HostName]
    , rPort    :: PortID
    , rAuth    :: Maybe B.ByteString
    , rKey     :: B.ByteString
    , rTimeout :: Int
    }
    deriving (Show)

data ConfigSegment = InputSegment Input | OutputSegment Output
    deriving (Show)

-- We want to deepseq the config before it is handed back to the shipper to
-- make use of. Don't want to fail when it's actually used.
instance NFData ConfigSegment where
     rnf (InputSegment a)  = a `deepseq` ()
     rnf (OutputSegment a) = a `deepseq` ()

instance NFData Input where
     rnf (FileInput a b) = a `deepseq` b `deepseq` ()
     rnf (ZMQ4Input a) = a `deepseq` ()

instance NFData ExtraInfo where
     rnf (ExtraString a) = a `deepseq` ()
     rnf (ExtraList a)   = a `deepseq` ()
     rnf (ExtraMap as)   = as `deepseq` ()

instance NFData Output where
     rnf (ZMQ4Output a b) = a `deepseq` b `deepseq` ()
     rnf Debug = ()
     -- Have to just `seq` PortID as it has no NFData instance
     rnf (Redis a c b d e) = 
        a `deepseq` b `deepseq` c `seq` d `deepseq` e `deepseq` () 

