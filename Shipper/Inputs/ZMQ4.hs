{-# LANGUAGE RecordWildCards #-}
module Shipper.Inputs.ZMQ4 (startZMQ4Input) where

import qualified Shipper.Types as Types
import Shipper.Event(readAllEvents)
import Control.Concurrent.STM
import Control.Concurrent.STM.TBQueue
import Control.Monad
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Blaze.ByteString.Builder
import qualified Data.ByteString as B

startZMQ4Input :: TBQueue Types.Event -> Types.Input -> Int -> IO ()
startZMQ4Input ch i@Types.ZMQ4Input{..} wait_time = runZMQ $ do
    s <- socket Rep
    bind s "tcp://*:5555"
    payload <- receive s
    liftIO $ print payload
    liftIO $ emit $ decode payload
  where
    emit = mapM_ (atomically . writeTBQueue ch)
    decode bs = map Types.PackedEvent $ MP.unpack bs
