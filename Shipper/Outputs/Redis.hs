{-# LANGUAGE RecordWildCards #-}
module Shipper.Outputs.Redis (startRedisOutput) where

import Shipper.Types
import Shipper.Event(readAllEvents)
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Data.MessagePack as MP
import Blaze.ByteString.Builder
import Control.Exception 
import Database.Redis
import System.Timeout

startRedisOutput :: TBQueue Event -> Int -> Output -> IO ()
startRedisOutput ch poll_period Redis{..} = loop rHosts
  where
    -- During normal operation, we simply loop through all of our hosts,
    -- sending a block of events to each in turn.
    loop hosts = do
        events <- readAllEvents ch
        trySend events hosts
    
    -- On failure, we keep trying to send that one block of events, clogging
    -- everyone's tubes up until we can transmit them.
    recover events hosts err = do
        putStrLn $ "Retrying send of " ++ (show . length) events ++ 
                   " events after redis push: " ++ show (err :: SomeException)
        threadDelay poll_period 
        trySend events hosts

    trySend events hosts = do
        (send events $ head hosts) `catch` (recover events $ shuffle hosts)
        loop $ shuffle hosts
      where shuffle h = last h : init h

    send [] _        = threadDelay poll_period -- Sending no events is easy!
    send events host = do
        t <- timeout rTimeout $ do
            conn <- connect $ connectionInfo host
            runRedis conn $ do
                -- We don't use pack here as that would give us a lazy
                -- bytestring, which we'd have to convert to strict to pass
                -- to rpush. This would be a massive fail.
                reply <- rpush rKey $ map (toByteString . MP.from) events
                case reply of
                    Left r -> error $ 
                        "Failed to rpush to " ++ host ++ ": "++ show r
                    _ -> return ()
        case t of
            Just v -> return v
            Nothing -> error $ "Timeout connecting to " ++ host

    connectionInfo host = ConnInfo
        { connectHost           = host
        , connectPort           = rPort
        , connectAuth           = rAuth
        , connectMaxIdleTime    = 30
        , connectMaxConnections = length rHosts
        }
