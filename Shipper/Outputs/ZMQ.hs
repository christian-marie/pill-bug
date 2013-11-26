module Shipper.Outputs.ZMQ (startZMQOutput) where

import Shipper.Types
import Shipper.Event(readAllEvents)
import Control.Concurrent.STM (atomically)
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.MessagePack as MP
import qualified Data.ByteString.Lazy as B

-- Output to 0MQ, compressing with lz4 and encrypting 
startZMQOutput :: TBQueue Event -> Int -> IO ()
startZMQOutput ch poll_period = do
    events <- readAllEvents ch
        
    send events

    threadDelay poll_period
    startZMQOutput ch poll_period
  where
    send e
        | null e    = return ()
        | otherwise = putStrLn $ show $ head e
