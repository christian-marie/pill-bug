{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Shipper.Outputs.ZMQ4 (startZMQ4Output) where

import qualified Shipper.Types as Types
import Shipper.Event(readAllEvents)
import Control.Concurrent
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Control.Applicative ((<$>))
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Blaze.ByteString.Builder
import Control.Exception
import System.Random
import Data.Restricted
import Data.Maybe
import qualified Data.ByteString as B
import qualified Codec.Compression.LZ4 as LZ4

-- How many 'packets' we usually send before we rotate to a different server.
rotationChance :: Int
rotationChance = 4

-- Output to 0MQ, compressing with lz4 and encrypting 
startZMQ4Output :: TBQueue Types.Event -> Int -> Types.Output -> IO ()
startZMQ4Output ch wait_time zo = do
    forever $ do
        k <- curveKeyPair
        catch (loop ch k wait_time zo)
              (\e -> do putStrLn $ "ZMQ output failed: " ++ 
                                    show (e :: SomeException)
                        threadDelay wait_time)

-- This function is much the same as startZMQ4Output, but it takes our
-- 'permanent' keypair generated above. (Only permanent for this ZMQ context,
-- but that's okay!)
--
-- We need to keep the key around for anything that uses the context as ZMQ
-- will fail to connect if we change it.
loop :: TBQueue Types.Event
        -> (Restricted Div5 B.ByteString, Restricted Div5 B.ByteString)
        -> Int 
        -> Types.Output 
        -> IO a
loop ch (pub, priv) wait_time Types.ZMQ4Output{..} = do
    forever $ do 
        server <- randomServer
        catch (runContext server) (zmqFailure server)
  where
    runContext server' = runZMQ $ do
        s <- openServer $ server'
        tryServer (s, server')

    zmqFailure :: String -> ZMQError -> IO ()
    zmqFailure s e = do
        putStrLn $ "ZMQ output failure with '" ++ show s ++ "': " ++ show e
        threadDelay wait_time

    randomServer = do 
        i <- getStdRandom $ randomR (0, (length zoServers - 1))
        return $ zoServers !! i

    tryServer s = do
        es <- liftIO $ readAllEvents ch
        if null es then liftIO $ threadDelay wait_time
                    else trySend s $ (compress . encode) es

        time_to_rotate <- liftIO timeToRotate
        unless time_to_rotate $ tryServer s
        where
        -- Convert to a [ByteString] before encoding that array of
        -- bytestrings. The other end will just take these encoded
        -- bytestrings straight back out and pass them on to whatever
        -- expects the msgpack codec at the destination end, without
        -- re-serialisation
        encode es = f $ map f es 
            where f o = (toByteString . MP.from) o

        -- We use high compression mode as it is even faster to decompress
        -- on the other end and I don't mind using a few extra cycles on
        -- these fringe nodes.
        compress bs = fromMaybe (error "LZ4.compress failed") $
                                LZ4.compressHC bs

        timeToRotate = (== 0) <$> (getStdRandom $ randomR (0, rotationChance))

    openServer server = do 
        s <- socket Req

        -- Setup crypto, building a throwaway keypair for this session. I
        -- don't care to use the long term client key for any form of
        -- authentication, so why make people configure it?
        setCurveServerKey TextFormat zoPublicKey s
        setCurvePublicKey TextFormat pub s
        setCurveSecretKey TextFormat priv s

        -- No lingering of sent messages on zmq_close(), this avoids duplicate
        -- messages as much as possible.
        setLinger (restrict (0 :: Integer)) s

        connect s server
        return s

    -- If the send works, great, don't touch anything.
    -- If it fails, we try the next server in the list.
    trySend (s,server) payload = do
        send s [] payload
        res <- poll zoTimeout [Sock s [In] Nothing] 
        if (null . head) res then do
            close s
            recover server payload zoTimeout 
        else
            void $ receive s

    -- Exponential backoff up to 30 seconds whilst shuffling through
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
    recover failed payload timeout = do
        liftIO $ putStrLn $ "ZMQ timeout transmitting to " ++ failed

        new_server <- liftIO randomServer
        s <- openServer new_server

        send s [] payload

        res <- poll timeout [Sock s [In] Nothing] 
        if (null . head) res then do
            close s
            recover new_server payload $ min (timeout * 2) 30000
        else do
            receive s
            close s
            liftIO $ putStrLn "ZMQ output recovered"

        -- Execution resumes in 'loop', on another random server.
