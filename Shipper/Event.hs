module Shipper.Event (
    readAllEvents
)
where

import Shipper.Types
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TBQueue

readAllEvents :: TBQueue Event -> STM [Event]
readAllEvents ch = do
    me <- tryReadTBQueue ch
    case me of
        Nothing -> return []
        Just e -> do
            rest <- readAllEvents ch
            return (e : rest)

