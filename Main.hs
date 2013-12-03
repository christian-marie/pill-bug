module Main (main) where

import Shipper
import Shipper.ConfigParser (parseConfig)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.ZMQ4.Monadic (curveKeyPair)
import Data.Restricted
import qualified Data.ByteString.Char8 as B

main :: IO ()
main = start =<< getArgs
  where
    start ["-k"] = do
        (pub, priv) <- curveKeyPair
        putStrLn "This private key goes in the input config:"
        B.putStrLn $ rvalue priv
        putStrLn "\nThis public key goes in the output config:"
        B.putStrLn $ rvalue pub
        putStrLn "\nPlease ensure that you quote both keys in the config."

    start ["-h"] = help

    start [config_path] = startShipper =<< parseConfig config_path

    start _ = help
    help    = do
        name <- getProgName
        putStrLn $ "Usage: " ++ name ++ " CONFIG"
        putStrLn $ "   OR  " ++ name ++ " -k (to generate a new key pair)"
        exitFailure

