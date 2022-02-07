module Data.Timeframe where

import Data.Interval
import Data.Time.Compat

type Timeframe = Interval UTCTime

localTimeframeAt :: TimeZone -> LocalTime -> LocalTime -> Timeframe
localTimeframeAt = on (:||:) . localTimeToUTC

localTimeframe :: LocalTime -> LocalTime -> IO Timeframe
localTimeframe t1 t2 =
  getCurrentTimeZone <&> \tz -> localTimeframeAt tz t1 t2

duration :: Timeframe -> Maybe NominalDiffTime
duration = measuring diffUTCTime
