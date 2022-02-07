module Data.Timeframe where

import Data.Interval
import Data.Time.Compat

type Timeframe = Interval UTCTime

duration :: Timeframe -> Maybe NominalDiffTime
duration = measuring diffUTCTime
