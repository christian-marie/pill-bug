module Shipper.Outputs.ZMQ (startZMQOutput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B

-- Output to 0MQ, compressing with lz4 and encrypting 
startZMQOutput :: TBQueue Event -> IO ()
startZMQOutput ch = forever $ do 
    event <- (atomically . readTBQueue) ch 
    B.writeFile "/tmp/laste" $ packMsg event

packMsg :: Event -> B.ByteString
packMsg e = MP.pack $ e
