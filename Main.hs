module Main (main) where

import Shipper
main :: IO ()
main = do
    -- TODO: load config from file
    let config = Config {
        inputs  = [
            FileInput 
                {filePath = "/tmp/log"
                ,fTags = ["tag"]
                ,fType = "redis" 
                }
        ],
        outputs = [Debug]
    }

    startShipper config

    return ()
        
