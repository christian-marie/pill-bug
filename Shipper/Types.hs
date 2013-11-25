module Shipper.Types (
    Event(..),
    Input(..),
    Output(..),
    ConfigSegment(..),
)
where

import qualified Data.ByteString.Char8 as B
import Data.Time
import Control.DeepSeq

data Event = Event
    { message :: B.ByteString
    , source  :: String
    , tags    :: [String]
    , tipe    :: String
    , time    :: UTCTime
    } deriving (Show)

data Input = FileInput
    { fTags     :: [String]
    , fType     :: String
    , filePaths :: [String] -- May be globs
    } deriving (Show)

data Output = Debug
    deriving (Show)

data ConfigSegment = InputSegment Input | OutputSegment Output
    deriving (Show)

instance NFData ConfigSegment where
     rnf (InputSegment a)  = a `deepseq` ()
     rnf (OutputSegment a) = a `deepseq` ()

instance NFData Input where
     rnf (FileInput a b c) = a `deepseq` b `deepseq` c `deepseq` ()

instance NFData Output where
     rnf Debug = ()

