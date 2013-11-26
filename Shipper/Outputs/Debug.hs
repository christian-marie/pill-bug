module Shipper.Outputs.Debug (startDebugOutput) where

import Shipper.Types
import Shipper.Event(readAllEvents)
import Control.Concurrent.STM.TBQueue
import Control.Monad

startDebugOutput :: TBQueue Event -> IO ()
startDebugOutput ch = forever $ readAllEvents ch >>= mapM_ print
