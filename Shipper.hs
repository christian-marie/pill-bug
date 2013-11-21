module Shipper (
    startShipper,
    Event,
    ShipperConfig(..),
    Input(..),
    Output(..),
) where

import Shipper.Inputs
import Shipper.Types

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TBQueue 
import Control.Concurrent (forkIO, ThreadId)
import Control.Monad (forever)


pollPeriod :: Int
pollPeriod = 100000

queueSize :: Int
queueSize = 10

startShipper :: ShipperConfig -> IO ()
startShipper config = do
    let inputs' = inputs config
    ch <- atomically $ newTBQueue queueSize

    -- start a thread for each input
    mapM_ (inputThread ch) inputs'

    -- TODO: send events to outputs, not print
    forever $ do
        event <- atomically $ readTBQueue ch
        print event

-- Handle FileInput inputs, initially we seek to the end of the file
inputThread :: TBQueue Event -> Input -> IO ThreadId 
inputThread ch input = forkIO $ case input of
    (FileInput _ _ _ ) -> readFileInput ch input pollPeriod
