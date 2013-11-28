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
startZMQ4Output ch wait_time Types.ZMQ4Output{..} = loop ["tcp://localhost:1234", "tcp://localhost:5555" ]
  where
    loop servers = do
        runZMQ $ do
            s <- openServer $ head servers
            tryServer (s, head servers) -- pass around the "name" of the server
        `catch` \e -> do
            putStrLn $ "ZMQ output failure: " ++ show (e :: ZMQError)
            threadDelay wait_time
            loop $ shuffle servers
      where

        tryServer s = do
            forever $ do
                es <- liftIO $ readAllEvents ch
                transmit s es

        openServer server = do 
            s <- socket Req
            connect s server
            return s

        transmit s es
            | null es   = liftIO $ threadDelay wait_time
            | otherwise = trySend s $ encode es
        encode es =
            let bss = map (toByteString . MP.from) es in
                (toByteString . MP.from) bss
        shuffle ss = last ss : init ss

        trySend (s,server) payload = do
            let to = 1000
            send s [] payload
            res <- poll to [Sock s [In] Nothing] 
            if (null . head) res then recover server servers (to * 2) payload
            else void $ receive s

        recover failed servers' to payload = do
                liftIO $ putStrLn $ "ZMQ timeout transmitting to: " ++ failed

                let shuffled = shuffle servers'
                let server  = head shuffled
                s <- openServer server

                send s [] payload
                res <- poll to [Sock s [In] Nothing] 
                if (null . head) res then recover server servers' (to * 2) payload 
                else void $ receive s

                close s

                liftIO $ loop shuffled
