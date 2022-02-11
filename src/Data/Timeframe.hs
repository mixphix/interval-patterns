module Data.Timeframe where

import Data.Interval
import Data.Interval.Set (IntervalSet)
import Data.Interval.Set qualified as IS
import Data.Map.Strict qualified as Map
import Data.Time.Compat
import GHC.IO (unsafePerformIO)

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

type Event = IntervalSet UTCTime

event :: Timeframe -> Event
event = IS.singleton

singleton :: (Ord e) => e -> Event -> Calendar e
singleton e ev = Calendar (Map.singleton e ev)

calendar :: (Ord e) => e -> Timeframe -> Calendar e
calendar e tf = singleton e (IS.singleton tf)

addEvent :: (Ord e) => e -> Event -> Calendar e -> Calendar e
addEvent e ev (Calendar c) = Calendar (Map.insertWith (<>) e ev c)

newtype Calendar e = Calendar {getCalendar :: Map e Event}
  deriving (Eq, Ord, Show, Typeable)

instance (Ord e) => Semigroup (Calendar e) where
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord e) => Monoid (Calendar e) where
  mempty = Calendar mempty

totalDuration :: (Ord e) => e -> Calendar e -> Maybe NominalDiffTime
totalDuration e (Calendar c) = case c Map.!? e of
  Nothing -> Just 0
  Just is -> foldr f (Just 0) (IS.intervalSet is)
  where
    f :: Timeframe -> Maybe NominalDiffTime -> Maybe NominalDiffTime
    f _ Nothing = Nothing
    f tf (Just x) = case duration tf of
      Nothing -> Nothing
      Just y -> Just (x + y)
