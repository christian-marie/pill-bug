module Shipper.Event (
    readAllEvents
)
where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent

-- This is the maximum number of events that an output will try to stick into
-- one packet. Assuming they're doin' it right.
--
-- TODO: Tune this to a good match with ZMQ over low latency intertube in mind.
maxPacketSize :: Integer
maxPacketSize = 1024

-- This was originally in the STM monad, however was moved to IO just to call
-- yield. The yield is needed when testing with a channel size of one, to
-- ensure that no extremely non-linear performance characteristics emerge.
--
-- We call this in order to read at most a packet sized array of events from
-- the given channel.
readAllEvents :: TBQueue Event -> IO [Event]
readAllEvents = readAllEvents' maxPacketSize
  where
    readAllEvents' :: Integer -> TBQueue Event -> IO [Event]
    readAllEvents' 0 _ = return []
    readAllEvents' current_size ch = do
        me <- atomically $ tryReadTBQueue ch
        case me of
            Nothing -> do
                let s = (maxPacketSize - current_size)
                if s /= 0 then do
                    print $ "Packet size: " ++ show s 
                    return []
                else return []
            Just e -> do
                -- This allows other (read: input) threads control over
                -- execution, which, worst case scenario is handed off to
                -- another output thread.
                --
                -- This should be fine due to the round robin nature of the
                -- current RTS scheduler in GHC.
                --
                -- Should the scheduler (theoretically) become pathologically
                -- biased towards an output thread, there's not much we can do
                -- here to achieve a full packet 100% of the time.
                --
                -- But the scheduler would have to become really really dumb
                -- for that. And I'm super lucky like that.
                yield
                rest <- readAllEvents' (current_size - 1) ch
                return (e : rest)

