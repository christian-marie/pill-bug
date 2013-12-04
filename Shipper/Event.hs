module Shipper.Event (
    readAllEvents,
    maxPacketSize

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
maxPacketSize :: Int
maxPacketSize = 1024

allowedTime :: Int
allowedTime = 100000 -- 100ms

-- This was originally in the STM monad, however was moved to IO just to call
-- yield. The yield is needed when testing with a channel size of one, to
-- ensure that no extremely non-linear performance characteristics emerge.
--
-- We call this in order to read at most a packet sized array of events from
-- the given channel.
readAllEvents :: TBQueue ChannelPayload -> IO [Event]
readAllEvents ch = readAllEvents' ch maxPacketSize allowedTime
  where
    readAllEvents' :: TBQueue ChannelPayload -> Int -> Int -> IO [Event]
    readAllEvents' _ 0 _ = return []
    readAllEvents' _ _ 0 = return []
    readAllEvents' ch' current_size start_time = do
        me <- atomically $ tryReadTBQueue ch'
        case me of
            Nothing -> let delay = 10000 in do -- 10ms
                threadDelay delay
                readAllEvents' ch current_size (start_time - delay)
            Just payload  -> do
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
                rest <- readAllEvents' ch (current_size - 1) start_time
                case payload of Single e    -> return $ e : rest
                                Multiple es -> return $ es ++ rest

