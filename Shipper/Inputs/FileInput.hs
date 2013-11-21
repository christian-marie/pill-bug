{-# LANGUAGE OverloadedStrings #-}
module Shipper.Inputs.FileInput (readFileInput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import System.IO

readFileInput :: TBQueue Event -> Input -> Int -> IO ()
readFileInput ch (FileInput _ _ path) wait_time =
    getFileEnd >>= emitFrom
  where
    emitFrom :: Integer -> IO ()
    emitFrom pos = do
        (nread, lines') <- linesFrom pos

        let events = map (newEvent path) lines' in 
            atomically $ mapM_ (writeTBQueue ch) events

        threadDelay wait_time
        emitFrom (fromIntegral nread + pos)

    linesFrom :: Integer -> IO (Int, [B.ByteString])
    linesFrom pos = withFile path ReadMode $ \h -> do
            hSeek h AbsoluteSeek pos
            bytes <- B.hGetContents h
            return (B.length bytes, B.lines bytes)

    getFileEnd :: IO Integer
    getFileEnd = withFile path ReadMode hFileSize
