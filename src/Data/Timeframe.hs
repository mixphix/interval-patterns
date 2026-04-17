module Data.Timeframe (
  Timeframe,
  module Data.Interval,
  localTimeframeAt,
  localTimeframe,
  pureLocalTimeframe,
  duration,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Function
import Data.Functor ((<&>))
import Data.Interval
import Data.Maybe
import Data.Time.Compat
import GHC.IO (unsafePerformIO)

import Bolt.Math

-- | > type Timeframe = Interval UTCTime
type Timeframe = Interval UTCTime

localTimeframeAt :: TimeZone -> LocalTime -> LocalTime -> Timeframe
localTimeframeAt = on (:||:) . localTimeToUTC

localTimeframe :: (MonadIO io) => LocalTime -> LocalTime -> io Timeframe
localTimeframe t1 t2 =
  liftIO getCurrentTimeZone <&> \tz -> localTimeframeAt tz t1 t2

pureLocalTimeframe :: LocalTime -> LocalTime -> Timeframe
pureLocalTimeframe t1 t2 =
  let tz = unsafePerformIO getCurrentTimeZone
   in localTimeframeAt tz t1 t2

duration :: Timeframe -> Maybe NominalDiffTime
duration = measuring (flip (-.))
