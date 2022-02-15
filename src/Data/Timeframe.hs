module Data.Timeframe
  ( Timeframe,
    module Data.Interval,
    localTimeframeAt,
    localTimeframe,
    pureLocalTimeframe,
    duration,
    Event,
    event,
    Calendar (..),
    singleton,
    calendar,
    addEvent,
    totalDuration,
  )
where

import Data.Interval
import Data.Interval.Layers (Layers)
import Data.Interval.Layers qualified as Layers
import Data.Map.Strict qualified as Map
import Data.Time.Compat
import GHC.IO (unsafePerformIO)

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
duration = measuring diffUTCTime

-- | An 'Event' is something that happens for a period of time.
--
-- > type Event = Layers UTCTime
type Event = Layers UTCTime

event :: Timeframe -> Event
event = Layers.singleton

newtype Calendar ev = Calendar {getCalendar :: Map ev Event}
  deriving (Eq, Ord, Show, Typeable)

instance (Ord ev) => Semigroup (Calendar ev) where
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord ev) => Monoid (Calendar ev) where
  mempty = Calendar mempty

singleton :: (Ord ev) => ev -> Event -> Calendar ev
singleton ev cvg = Calendar (Map.singleton ev cvg)

calendar :: (Ord ev) => ev -> Timeframe -> Calendar ev
calendar ev tf = singleton ev (Layers.singleton tf)

addEvent :: (Ord ev) => ev -> Event -> Calendar ev -> Calendar ev
addEvent ev cvg (Calendar c) = Calendar (Map.insertWith (<>) ev cvg c)

totalDuration :: (Ord ev) => ev -> Calendar ev -> Maybe NominalDiffTime
totalDuration ev (Calendar c) = case c Map.!? ev of
  Nothing -> Just 0
  Just is -> foldr f (Just 0) (Layers.toList is)
  where
    f :: (Int, Timeframe) -> Maybe NominalDiffTime -> Maybe NominalDiffTime
    f _ Nothing = Nothing
    f (n, tf) (Just x) = case (fromIntegral n *) <$> duration tf of
      Nothing -> Nothing
      Just y -> Just (x + y)
