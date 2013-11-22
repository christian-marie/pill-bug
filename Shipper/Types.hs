  module Shipper.Types (
      Event(..),
      Input(..),
      Output(..),
      ShipperConfig(..),
  )
  where
  
  import qualified Data.ByteString.Char8 as B
  import Data.Time
  
  data Event = Event
      { message :: B.ByteString
      , source  :: String
      , tags    :: [String]
      , tipe    :: String
      , time    :: UTCTime
      } deriving (Show)
  
  data Input = FileInput
      { fTags    :: [String]
      , fType    :: String
      , filePath :: FilePath
      } deriving (Show)
  
  data Output = Debug
  
  data ShipperConfig = Config
      { inputs  :: [Input]
      , outputs :: [Output]
      }
