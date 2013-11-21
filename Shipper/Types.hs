module Shipper.Types (
    Event(..),
    Input(..),
    Output(..),
    ShipperConfig(..),
    newEvent,
)
where

import qualified Data.ByteString.Char8 as B

data Event = Event
    { message :: B.ByteString
    , source  :: String
    } deriving (Show)

data Input = FileInput
    { tags     :: [String]
    , tipe     :: String -- type
    , filePath :: FilePath
    } deriving (Show)

data Output = Debug

data ShipperConfig = Config
    { inputs  :: [Input]
    , outputs :: [Output]
    }

-- TODO: timestamp, tags, type
newEvent :: String -> B.ByteString -> Event
newEvent s m = Event
    { message = m
    , source  = s
    }

