{-# LANGUAGE RecordWildCards #-}
module Shipper.Outputs.ZMQ4 (startZMQ4Output) where

import qualified Shipper.Types as Types
import Shipper.Event(readAllEvents)
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Blaze.ByteString.Builder
import Control.Exception

-- Output to 0MQ, compressing with lz4 and encrypting 
startZMQ4Output :: TBQueue Types.Event -> Int -> Types.Output -> IO ()
startZMQ4Output ch wait_time Types.ZMQ4Output{..} = loop zoServers 
  where
    loop servers = do
        -- Open a context with the first server in the list
        runZMQ $ do
            s <- openServer $ head servers
            tryServer (s, head servers)
        `catch` \e -> do
            -- These are the only exceptions that should be coming out of the
            -- ZMQ4 monad
            putStrLn $ "ZMQ output failure with '" ++ head servers ++
                "': " ++ show (e :: ZMQError)
            threadDelay wait_time
            loop $ shuffle servers
      where

        tryServer s = do
            forever $ do
                -- TODO:
                -- non-pseudo randomly shuffle the server list and jump to
                -- another one for load balancing every now and then.
                es <- liftIO $ readAllEvents ch
                if null es then liftIO $ threadDelay wait_time
                           else trySend s $ encoded es
          where
            -- Convert to a [ByteString] before encoding that array of
            -- bytestrings. The other end will just take these encoded
            -- bytestrings straight back out and pass them on to whatever
            -- expects the msgpack codec at the destination end, without
            -- re-serialisation
            encoded es = f $ map f es 
              where f o = (toByteString . MP.from) o


        openServer server = do 
            s <- socket Req
            connect s server
            return s

        shuffle ss = last ss : init ss

        -- If the send works, great, don't touch anything.
        -- If it fails, we try the next server in the list.
        trySend (s,server) payload = do
            send s [] payload
            res <- poll zoTimeout [Sock s [In] Nothing] 
            if (null . head) res then recover server servers payload zoTimeout 
                                 else void $ receive s

        -- Exponential backoff up to 60 seconds whilst shuffling through
        -- avaliable servers. If a server replies, we carry on with that one
        -- from now on.
        --
        -- This doesn't allow for any real load balancing, but does work very
        -- well in the event of a dead server. 
        --
        -- We can achieve load balancing later either simply, by connecting to
        -- a random server every now and then.
        --
        -- If we really want to ramp up complexity and bandwith usage for no
        -- real benifit, maybe implement:
        -- http://zguide.zeromq.org/php:chapter4#Model-Three-Complex-and-Nasty
        recover failed servers' payload timeout = do
                liftIO $ putStrLn $ "ZMQ timeout transmitting to: " ++ failed

                let shuffled = shuffle servers'
                let server  = head shuffled
                s <- openServer server

                send s [] payload

                res <- poll timeout [Sock s [In] Nothing] 
                if (null . head) res then
                    recover server shuffled payload $ min (timeout * 2) 60000
                else
                    void $ receive s

                close s
                liftIO $ loop shuffled
