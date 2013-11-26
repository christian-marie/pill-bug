module Shipper (
    startShipper,
    Event,
    Input(..),
    Output(..),
) where

import Shipper.Inputs
import Shipper.Outputs
import Shipper.Types
import Shipper.Event(readAllEvents)

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue 
import Control.Concurrent
import Control.Monad 


pollPeriod :: Int
pollPeriod = 100000 -- 100ms

-- Test with a small queue size to uncover race conditions
-- In production, we can never send more than queueSize events per pollPeriod
-- as the outputs attempt to flush the queue completely every pollPeriod.
--
-- So, for a cap of 1000 events per second (bottlenecked at outputs),
-- we have a queue size of 500 and a pollPeriod of 100000 (100ms)
--
-- 500, as we have the input queue + the output queue
queueSize :: Int
--queueSize = 500
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
            Debug -> forkIO $ startDebugOutput out_chan pollPeriod
            ZMQ   -> forkIO $ startZMQOutput out_chan pollPeriod
        return out_chan

    forever $ do
        -- For every event that comes in, try to send it to every output
        -- channel. This way, if an output gets clogged we can block all the
        -- way back to every input magically, and no output should get more
        -- than one event more than another.
        events <- atomically $ readAllEvents in_ch
        forM_ events $ \e -> 
            forM_ out_chs $ \ch -> atomically $ writeTBQueue ch e

        threadDelay pollPeriod
  where
    isInputSegment (InputSegment _) = True
    isInputSegment _                = False

    isOutputSegment (OutputSegment _) = True
    isOutputSegment _                 = False

    inputSegments = filter isInputSegment segments
    outputSegments = filter isOutputSegment segments
