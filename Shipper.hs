module Shipper (
    startShipper,
    Event,
    ShipperConfig(..),
    Input(..),
    Output(..),
) where

import Shipper.Inputs
import Shipper.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue 
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_)


pollPeriod :: Int
pollPeriod = 100000

-- Test with a small queue size to uncover race conditions
queueSize :: Int
queueSize = 1

startShipper :: ShipperConfig -> IO ()
startShipper config = do
    let inputs' = inputs config
    ch <- atomically $ newTBQueue queueSize

    -- A thread for each input
    forM_ inputs' $ \i -> forkIO $ case i of 
        FileInput _ _ _ -> readFileInput ch i pollPeriod

    -- TODO: send events to outputs, not print
    forever $ do
        event <- atomically $ readTBQueue ch
        print event

