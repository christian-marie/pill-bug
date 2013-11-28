module Shipper.Outputs.Debug (startDebugOutput) where

import Shipper.Types
import Shipper.Event(readAllEvents)
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Control.Concurrent

startDebugOutput :: TBQueue Event -> Int -> IO ()
startDebugOutput ch wait_time = forever $ printEvents =<< readAllEvents ch
  where
    printEvents [] = threadDelay wait_time
    printEvents es = mapM_ print es
