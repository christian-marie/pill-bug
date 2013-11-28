{-# LANGUAGE RecordWildCards #-}
module Shipper.Inputs.ZMQ4 (startZMQ4Input) where

import qualified Shipper.Types as Types
import Control.Concurrent.STM
import Control.Monad
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Control.Exception
import qualified Data.ByteString as B

startZMQ4Input :: TBQueue Types.Event -> Types.Input -> Int -> IO ()
startZMQ4Input ch Types.ZMQ4Input{..} _ = forever $ do
    runZMQ $ do
        s <- socket Rep
        bind s "tcp://*:5555"
        forever $ do
            payload <- receive s
            liftIO $ emit $ decode payload
            send s [] B.empty
    `catch` \e -> do
        putStrLn $ "ZMQ input server died: " ++ show (e :: SomeException)
  where
    emit      = mapM_ (atomically . writeTBQueue ch)
    decode bs = map Types.PackedEvent $ MP.unpack bs
