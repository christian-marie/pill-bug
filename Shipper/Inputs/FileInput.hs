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
import qualified Control.Concurrent.ThreadManager as TM

-- How much data to try to read at a time.
chunkSize :: Int
chunkSize = 1048576 -- 1MB

-- How long to keep reading a file after it is rotated.
rotationWait :: Int
rotationWait = 3000000 -- 3 seconds

-- How often to re-evaluate globs and check on the health of all threads
globRate :: Int
globRate = 1000000

readFileInput :: TBQueue Event -> Input -> Int -> IO ()
readFileInput ch input@FileInput{..} wait_time = do
    manager <- TM.make
    manageThreads manager ([] :: [(FilePath, ThreadId)])
  where
    -- Constantly glob all filePaths, ensuring that each globbed item has one
    -- healthy thread running.
    manageThreads :: TM.ThreadManager -> [(FilePath, ThreadId)] -> IO ()
    manageThreads manager threads = do
        files <- expandGlobs filePaths

        -- This gets a bit unreadable here, but bear with me:
        -- For each globbed file, we check if there is a thread registered
        new_threads <- forM files $ \f -> case lookup f threads of
            -- In the event of one being regisered, we check the status of it
            Just tid -> TM.getStatus manager tid >>= \s -> case s of 
                -- Given a successful lookup:
                Just status -> case status of
                    -- Restart if the thread exited normally (shouldn't happen)
                    TM.Finished -> do
                        putStrLn $ f ++ "Thread stopped, restarting."
                        (,) f `liftM` newThread manager f

                    -- Restart if the thread died, this will happen.
                    TM.Threw e -> do
                        putStrLn $ "File input: " ++ show e
                        (,) f `liftM` newThread manager f

                    -- And carry on if everything is okay.
                    TM.Running -> return (f, tid)

                -- This bit shouldn't happen, start one anyway.
                Nothing -> (,) f `liftM` newThread manager f
                    
            -- Thread isn't registered yet, start one
            Nothing -> (,) f `liftM` newThread manager f
        
        threadDelay globRate
        manageThreads manager new_threads

        
    newThread manager file = TM.fork manager $ readThread ch input wait_time file

-- Take a list of globs like [ "/tmp/*.log", "/actual_file" ] and expand them
-- 
-- / here is not portable. I don't care.
expandGlobs :: [FilePath] -> IO [FilePath]
expandGlobs fps = (concat . fst) `liftM` globDir (map compile fps) "/"

-- Start the log watching process by opening the file and seeking to end.
readThread ::  TBQueue Event -> Input -> Int -> FilePath -> IO ()
readThread ch FileInput{..} wait_time log_path = 
    withFile log_path ReadMode $ \h -> do
        inode <- getFileID log_path
        hSeek h SeekFromEnd 0
        readLog h inode 
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
