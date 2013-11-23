module Main (main) where

import Shipper
import Shipper.Types
import Shipper.ConfigParser (parseConfig)
import Control.Monad (unless)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import Control.DeepSeq

main :: IO ()
main = do
    args <- getArgs 
    unless (length args == 1) $ do 
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " CONFIG"
        exitFailure

    c <- parseConfig (head args)
    unless (any isInputSegment c) $ error "No inputs specified"

    print c

    c `deepseq` startShipper c
  where
    isInputSegment (InputSegment _) = True
    isInputSegment _ = False
