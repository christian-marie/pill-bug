module Main (main) where

import Shipper
import Shipper.ConfigParser (parseConfig)
import Control.Monad (unless)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs 
    unless (length args == 1) $ do 
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " CONFIG"
        exitFailure

    startShipper =<< parseConfig (head args)
