module Shipper.Outputs.Debug (startDebugOutput) where

import Shipper.Types
import Shipper.Event(readAllEvents)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import Control.Monad

startDebugOutput :: TBQueue Event -> Int -> IO ()
startDebugOutput ch poll_period = forever $ do
    events <- readAllEvents ch
    mapM_ print events
    threadDelay poll_period
