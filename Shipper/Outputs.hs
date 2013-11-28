module Shipper.Outputs (
    startDebugOutput,
    startZMQ4Output,
    startRedisOutput,
) where

import Shipper.Outputs.Debug
import Shipper.Outputs.ZMQ4
import Shipper.Outputs.Redis
