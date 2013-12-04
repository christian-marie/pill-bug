{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Shipper.Inputs.ZMQ4 (startZMQ4Input) where

import qualified Shipper.Types as Types
import Control.Concurrent.STM
import Control.Monad
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Control.Exception
import Control.Concurrent
import Data.Maybe
import qualified Data.ByteString as B
import qualified Codec.Compression.LZ4 as LZ4

startZMQ4Input :: TBQueue Types.ChannelPayload-> Types.Input -> Int -> IO ()
startZMQ4Input ch Types.ZMQ4Input{..} wait_time = forever $ do
    runZMQ $ do
        s <- socket Rep
        setCurveServer True s
        setCurveSecretKey TextFormat ziPrivateKey s
        bind s ziBind

        forever $ do
            payload <- receive s
            liftIO $ emit $ (decode . decompress) payload
            send s [] B.empty
    `catch` \e -> do
        putStrLn $ "ZMQ input server died: " ++ show (e :: SomeException)
        threadDelay wait_time
  where
    emit          = atomically . writeTBQueue ch . Types.Multiple
    decode bs     = map Types.PackedEvent $ MP.unpack bs
    decompress bs = fromMaybe (error "LZ4.decompress failed") $
                              LZ4.decompress bs
