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
import System.Random
import Control.Applicative
import Control.Monad.IO.Class

-- How many 'packets' we usually send before we rotate to a different server.
rotationChance :: Int
rotationChance = 1024

startRedisOutput :: TBQueue Event -> Int -> Output -> IO ()
startRedisOutput ch poll_period Redis{..} = loop =<< randomServer
  where
    randomServer = do 
        i <- getStdRandom $ randomR (0, (length rServers - 1))
        return $ rServers !! i

    -- During normal operation, we simply loop through all of our hosts,
    -- sending a block of events to each in turn.
    loop server = do
        events <- readAllEvents ch
        trySend events server
    
    -- On failure, we keep trying to send that one block of events, clogging
    -- everyone's tubes up until we can transmit them.
    recover events err = do
        putStrLn $ "Retrying send of " ++ (show . length) events ++ 
                   " events after redis push: " ++ show (err :: SomeException)
        threadDelay poll_period 
        trySend events =<< randomServer

    trySend events server = do
        (send events server) `catch` (recover events)

        -- Hop around servers every now and then, but generally stick with the
        -- one we know works for obvious performance reasons.
        now <- timeToRotate
        if now then loop =<< randomServer
               else loop server
      where
        timeToRotate = (== 0) <$> (getStdRandom $ randomR (0, rotationChance))

    send [] _        = threadDelay poll_period -- Sending no events is easy!
    send events (host, port) = do
        t <- timeout rTimeout $ do
            conn <- connect $ connectionInfo host port
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

    connectionInfo host port = ConnInfo
        { connectHost           = host
        , connectPort           = port
        , connectAuth           = rAuth
        , connectMaxIdleTime    = 30
        , connectMaxConnections = length rServers
        }
