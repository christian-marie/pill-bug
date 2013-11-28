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
import qualified Data.ByteString as B

-- Output to 0MQ, compressing with lz4 and encrypting 
startZMQ4Output :: TBQueue Types.Event -> Int -> Types.Output -> IO ()
startZMQ4Output ch wait_time o@Types.ZMQ4Output{..} = do
    readAllEvents ch >>= transmit
    startZMQ4Output ch wait_time o
  where
    transmit es
        | null es   = threadDelay wait_time
        | otherwise = sendZMQ4 $ encode es 
    encode es =
        let bss = map (toByteString . MP.from) es in
            (toByteString . MP.from) bss

sendZMQ4 :: B.ByteString -> IO ()
sendZMQ4 payload = runZMQ $ do
    s <- socket Req
    connect s "tcp://localhost:5555"
    liftIO $ print payload
    send s [] payload
    void $ receive s

