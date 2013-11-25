module Main (main) where

import Shipper
import Shipper.ConfigParser (parseConfig)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = start =<< getArgs
  where
    start [config_path] = startShipper =<< parseConfig config_path
    start _ = do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " CONFIG"
        exitFailure

