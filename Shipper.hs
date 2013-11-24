module Shipper (
    startShipper,
    Event,
    Input(..),
    Output(..),
) where

import Shipper.Inputs
import Shipper.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue 
import Control.Concurrent (forkIO)
import Control.Monad (forever, forM_, unless)


pollPeriod :: Int
pollPeriod = 100000

-- Test with a small queue size to uncover race conditions
queueSize :: Int
queueSize = 1

startShipper :: [ConfigSegment] -> IO ()
startShipper segments = do
    ch <- atomically $ newTBQueue queueSize

    unless (any isInputSegment segments) $ error "No inputs specified"
    
    -- Do something useful for each configuration segment
    forM_ segments $ \s -> case s of 
        InputSegment i -> case i of
            FileInput _ _ _ -> forkIO $ readFileInput ch i pollPeriod

    -- TODO: send events to outputs, not print
    forever $ do
        event <- atomically $ readTBQueue ch
        print event
  where
    isInputSegment (InputSegment _) = True
    isInputSegment _                = False
