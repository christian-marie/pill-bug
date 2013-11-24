{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables #-}
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
import Data.Time
import System.FilePath.Glob

-- How much data to try to read at a time.
chunkSize :: Int
chunkSize = 1048576 -- 1MB

-- How long to keep reading a file after it is rotated.
rotationWait :: Int
rotationWait = 3000000 -- 3 seconds

readFileInput :: TBQueue Event -> Input -> Int -> IO ()
readFileInput fp input@FileInput{..} wait_time = do
    files <- expandGlobs filePaths
    print files

-- Take a list of globs like [ "/tmp/*.log", "/actual_file" ] and expand them
-- 
-- / here is not portable. I don't care.
expandGlobs :: [FilePath] -> IO [FilePath]
expandGlobs fps = (concat . fst) `liftM` globDir (map compile fps) "/"

-- Start the log watching process by opening the file and seeking to end.
readThread :: FilePath -> TBQueue Event -> Input -> Int -> IO ()
readThread log_path ch input@FileInput{..} wait_time = 
    withFile log_path ReadMode $ \h -> do
        inode <- getFileID log_path
        hSeek h SeekFromEnd 0
        readLog h inode 
    `catch` \e -> do 
        -- Restart from the end in the event of an explosion. We can do this
        -- here or in our caller, I prefer here.
        print (e :: SomeException)
        threadDelay wait_time
        readThread log_path ch input wait_time
  where
    -- Read events whilst watching for log rotations, we know that a log has
    -- rotated when the inode of the log_path changes.
    readLog :: Handle -> FileID -> IO ()
    readLog h last_inode = do
        (inode, rotated) <- isRotated log_path last_inode
        if rotated
        then do 
            -- Daemons can still write to the now 'old' handle, so we need to
            -- continue reading for it for a little bit.
            forkIO $ do
                let n = rotationWait `quot` wait_time in
                    replicateM_ n $ emitFrom h >> threadDelay wait_time
                hClose h
            -- Continue reading from beginning of new file
            withFile log_path ReadMode $ \newh -> readLog newh inode 
        else do 
            -- During normal operation, just emit events, sleep and try again
            emitFrom h
            threadDelay wait_time
            readLog h inode
 
    -- Read some bytes from the handle, splitting them into lines, building
    -- events from those lines and then handing them off to the shipper.
    emitFrom :: Handle -> IO ()
    emitFrom h =
        B.lines `liftM` B.hGetSome h chunkSize 
        >>= mapM buildEvent
        >>= mapM_ (atomically . writeTBQueue ch)

    -- Tack on the time at the moment that the event is packaged up
    buildEvent :: B.ByteString -> IO Event
    buildEvent line = do
        t <- getCurrentTime
        return Event
            {message = line
            ,source  = log_path
            ,tags    = fTags
            ,tipe    = fType
            ,time    = t
            }

-- Check for an inode mismatch with the path
--
-- The error handling here is to deal with us being called at the the exact
-- moment that the log is rotated
isRotated :: FilePath -> FileID -> IO (FileID, Bool)
isRotated path last_inode =  do
    r <- try $ do 
        inode <- getFileID path
        return (inode, inode /= last_inode)
    case r of 
        Left (_ :: IOException) -> return (last_inode, False)
        Right result -> return result

-- Retrieve an inode number. This is not portable.
getFileID :: FilePath -> IO FileID
getFileID p = fileID `liftM` getFileStatus p
