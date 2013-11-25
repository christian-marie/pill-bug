module Shipper (
    startShipper,
    Event,
    Input(..),
    Output(..),
) where

import Shipper.Inputs
import Shipper.Outputs
import Shipper.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue 
import Control.Concurrent (forkIO)
import Control.Monad 


pollPeriod :: Int
pollPeriod = 100000

-- Test with a small queue size to uncover race conditions
queueSize :: Int
queueSize = 1

startShipper :: [ConfigSegment] -> IO ()
startShipper segments = do
    -- Events read from inputs come through this 'channel'
    in_ch <- atomically $ newTBQueue queueSize

    when (null inputSegments)  $ error "No inputs specified"
    when (null outputSegments) $ error "No outputs specified"
    
    -- Do something useful for each input segment, we hand all inputs the same
    -- channel to stream events over
    forM_ inputSegments $ \(InputSegment i) -> case i of 
        FileInput _ _ -> forkIO $ startFileInput in_ch i pollPeriod

    -- Output segments however, each get thier own channel. This is so that
    -- inputs all block when any given output blocks. That way we don't leak
    -- any memory and outputs don't get out of sync when a single output dies.
    out_chs <- forM outputSegments $ \(OutputSegment o) -> do 
        out_chan <- atomically $ newTBQueue queueSize
        case o of 
            Debug -> forkIO $ startDebugOutput out_chan
            ZMQ   -> forkIO $ startZMQOutput out_chan
        return out_chan

    forever $ do
        -- For every event that comes in, try to send it to every output
        -- channel. This way, if an output gets clogged we can block all the
        -- way back to every input magically.
        event <- atomically $ readTBQueue in_ch
        forM_ out_chs $ \ch -> atomically $ writeTBQueue ch event
  where
    isInputSegment (InputSegment _) = True
    isInputSegment _                = False

    isOutputSegment (OutputSegment _) = True
    isOutputSegment _                 = False

    inputSegments = filter isInputSegment segments
    outputSegments = filter isOutputSegment segments
