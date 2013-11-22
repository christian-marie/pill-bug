{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Shipper.Inputs.FileInput (readFileInput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString as B
import System.IO
import Control.Exception
import Control.Monad
import System.Posix.Files
import System.Posix.Types (FileID)

-- 1MB is entirely arbritrary and seems reasonable.
chunkSize :: Int
chunkSize = 1048576

readFileInput :: TBQueue Event -> Input -> Int -> IO ()
readFileInput ch input@FileInput{..} wait_time = 
    -- We start from the end of a given file
    withFile filePath ReadMode $ \h -> do
        inode <- getFileID filePath
        hSeek h SeekFromEnd 0
        readLog h inode 
    `catch` \e -> do 
        -- And restart from the end in the event of an explosion
        print (e :: SomeException)
        threadDelay wait_time
        readFileInput ch input wait_time
  where
    -- Read events whilst watching for log rotations
    readLog :: Handle -> FileID -> IO ()
    readLog h last_inode = do
        (inode, rotated) <- isRotated filePath last_inode
        if rotated
        then do 
            -- Try to read for three more seconds
            forkIO $ do
                let n = 3000000 `quot` wait_time in
                    replicateM_ n $ do
                        emitFrom h
                        threadDelay wait_time
                hClose h
            -- Continue reading from beginning of new file
            withFile filePath ReadMode $ \newh -> readLog newh inode 
        else do 
            -- During normal operation, just emit events, sleep and try again
            emitFrom h
            threadDelay wait_time
            readLog h inode
 
    emitFrom :: Handle -> IO ()
    emitFrom h = do
        lines' <- B.lines `liftM` B.hGetSome h chunkSize 
        let events = map (newEvent filePath) lines' in
            mapM_ (atomically . writeTBQueue ch) events

-- Check for an inode mismatch with the filefilePath
isRotated :: FilePath -> FileID -> IO (FileID, Bool)
isRotated filePath last_inode =  do
    r <- try $ do 
        inode <- getFileID filePath
        return (inode, inode /= last_inode)
    case r of 
        -- This is mostly to handle the case of us trying to read the inode
        -- number at the exact moment that the log is rotated
        Left e       -> print (e :: IOException) >> return (last_inode, False)
        Right result -> return result

getFileID :: FilePath -> IO FileID
getFileID p = fileID `liftM` getFileStatus p
