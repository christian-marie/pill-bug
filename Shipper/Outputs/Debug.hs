module Shipper.Outputs.Debug (startDebugOutput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Monad

startDebugOutput :: TBQueue Event -> IO ()
startDebugOutput ch = forever $ (atomically . readTBQueue) ch >>= print
