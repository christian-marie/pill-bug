module Main (main) where

import Shipper
main :: IO ()
main = do
    -- TODO: load config from file
    let config = Config {
        inputs  = [
            FileInput { filePath = "/tmp/log", tags = ["tag"], tipe = "redis" }
        ],
        outputs = [Debug]
    }

    startShipper config

    return ()
        
