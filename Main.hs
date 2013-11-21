module Main (main) where

import Shipper
main :: IO ()
main = do
    -- TODO: load config from file
    let config = Config {
        inputs  = [
            FileInput { filePath = "/var/log/redis.log", tags = ["tag"], tipe = "redis" },
            FileInput { filePath = "/var/log/daemon.log", tags = ["tag"], tipe = "redis" }
        ],
        outputs = [Debug]
    }

    startShipper config

    return ()
        
