{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables,
             TupleSections #-}
module Shipper.Inputs.File (startFileInput) where

import Shipper.Types
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue
import Control.Concurrent
import qualified Data.ByteString as B
import System.IO 
import Control.Exception
import Control.Monad
import Control.Applicative
import System.Posix.Files
import System.Posix.Types (FileID)
import Data.Time.LocalTime
import System.FilePath.Glob
import System.Directory (doesFileExist)
import Data.Time.RFC3339
import qualified Control.Concurrent.ThreadManager as TM
-- How long to keep reading a file after it is rotated.
rotationWait :: Int
rotationWait = 3000000 -- 3 seconds

-- How often to re-evaluate globs and check on the health of all threads
--
-- Globbing is actually surprisingly expensive, so I've dropped this down to 10
-- seconds.
globRate :: Int
globRate = 10000000 -- 10 seconds

startFileInput :: TBQueue Event -> Input -> Int -> IO ()
startFileInput ch input@FileInput{..} wait_time = do
    manager <- TM.make
    manageThreads manager ([] :: [(FilePath, ThreadId)])
  where
    -- Constantly glob all filePaths, ensuring that each globbed item has one
    -- healthy thread running.
    manageThreads :: TM.ThreadManager -> [(FilePath, ThreadId)] -> IO ()
    manageThreads manager threads = do
        files <- concat <$> mapM expandGlobs filePaths

        -- This gets a bit unreadable here, but bear with me:
        -- For each globbed file, we check if there is a thread registered
        new_threads <- forM files $ \file -> case lookup file threads of
            -- In the event of one being regisered, we check the status of it
            Just tid -> TM.getStatus manager tid >>= \s -> case s of 
                -- Given a successful lookup:
                Just status -> case status of
                    -- Restart if the thread exited normally (shouldn't happen)
                    TM.Finished -> do
                        putStrLn $ file ++ "Thread stopped, restarting."
                        (file,) <$> newThread manager file

                    -- Restart if the thread died, this will happen.
                    TM.Threw e -> do
                        putStrLn $ "File input: " ++ show e
                        (file,) <$> newThread manager file

                    -- And carry on if everything is okay.
                    TM.Running -> return (file, tid)

                -- This bit shouldn't happen, start one anyway.
                Nothing -> (file,) <$> newThread manager file
                    
            -- Thread isn't registered yet, start one
            Nothing -> (file,) <$> newThread manager file
        
        threadDelay globRate
        manageThreads manager new_threads

        
    newThread manager file = TM.fork manager $
        readThread ch input wait_time file

-- Take a single glob and expand it to all matches. We have to do globs one at
-- a time for efficiency as they don't always have the same common directory.
-- We also filter out all non-files, such as directories. This is so that you
-- can use a glob like /var/log/*
--
-- Not that it's a good idea.
expandGlobs :: FilePath -> IO [FilePath]
expandGlobs fp = do
    exists <- doesFileExist fp
    if exists then return [fp]
              else filterM doesFileExist =<< allMatches
  where
    pattern = compile fp
    dir = (fst . commonDirectory) pattern
    allMatches = (concat . fst) <$> globDir [pattern] dir

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
            -- During normal operation, just emit events
            emitFrom h
            threadDelay wait_time
            readLog h inode
 
    -- This used to read events in large chunks, it's been simplified to get
    -- one line at a time. This seems a lot more robust and a lot better on
    -- memory. It is a lot slower though.
    --
    -- It's plenty fast enough for any sane log souce though. If that ever
    -- prooves to be false, we can look into io-streams or pipes
    emitFrom :: Handle -> IO ()
    emitFrom h = do
        eof <- hIsEOF h
        if eof then
            return ()
        else do
            e <- B.hGetLine h >>= buildEvent
            atomically $ writeTBQueue ch e
            emitFrom h

    -- Tack on the time at the moment that the event is packaged up
    buildEvent :: B.ByteString -> IO Event
    buildEvent line = do
        t <- showRFC3339 <$> getZonedTime
        return UnpackedEvent
            {message = line
            ,extra   = fExtra
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
        Right result            -> return result

-- Retrieve an inode number. This is not portable.
getFileID :: FilePath -> IO FileID
getFileID p = fileID `liftM` getFileStatus p
