{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Shipper.Inputs.ZMQ4 (startZMQ4Input) where

import qualified Shipper.Types as Types
import Control.Concurrent.STM
import Control.Monad
import Data.MessagePack as MP
import System.ZMQ4.Monadic
import Control.Exception
import Control.Concurrent
import qualified Data.ByteString as B

startZMQ4Input :: TBQueue Types.Event -> Types.Input -> Int -> IO ()
startZMQ4Input ch Types.ZMQ4Input{..} wait_time = forever $ do
    runZMQ $ do
        s <- socket Rep
        let key = restrict "*=CG{(0Id/7&{Cx./k]xy[]3=D%lb3g/-vnfF&w+"
        setCurveServer True s
        setCurveSecretKey TextFormat key s
        
        bind s ziBind
        forever $ do
            payload <- receive s
            liftIO $ emit $ decode payload
            send s [] B.empty
    `catch` \e -> do
        putStrLn $ "ZMQ input server died: " ++ show (e :: SomeException)
        threadDelay wait_time
  where
    emit      = mapM_ (atomically . writeTBQueue ch)
    decode bs = map Types.PackedEvent $ MP.unpack bs
