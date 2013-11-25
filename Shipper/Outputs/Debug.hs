module Shipper.Outputs.Debug (startDebugOutput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Monad

startDebugOutput :: TBQueue Event -> IO ()
startDebugOutput event_ch = forever $ do
    event <- atomically $ readTBQueue event_ch
    print event
