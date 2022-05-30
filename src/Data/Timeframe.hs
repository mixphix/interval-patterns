module Data.Timeframe (
  Timeframe,
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
) where

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
-- > type Event n = Layers UTCTime (Sum n)
type Event n = Layers UTCTime (Sum n)

event :: (Num n) => Timeframe -> Event n
event = (`Layers.singleton` 1)

newtype Calendar ev n = Calendar {getCalendar :: Map ev (Event n)}
  deriving (Eq, Ord, Show, Typeable)

instance (Ord ev, Num n) => Semigroup (Calendar ev n) where
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord ev, Num n) => Monoid (Calendar ev n) where
  mempty = Calendar mempty

singleton :: (Ord ev, Num n) => ev -> Event n -> Calendar ev n
singleton ev cvg = Calendar (Map.singleton ev cvg)

calendar :: (Ord ev, Num n) => ev -> Timeframe -> Calendar ev n
calendar ev tf = singleton ev (Layers.singleton tf 1)

addEvent :: (Ord ev, Num n) => ev -> Event n -> Calendar ev n -> Calendar ev n
addEvent ev cvg (Calendar c) = Calendar (Map.insertWith (<>) ev cvg c)

totalDuration :: forall ev n. (Ord ev, Real n) => ev -> Calendar ev n -> Maybe NominalDiffTime
totalDuration ev (Calendar c) = case c Map.!? ev of
  Nothing -> Just 0
  Just is -> foldr f (Just 0) (Layers.toList is)
 where
  f :: (Timeframe, Sum n) -> Maybe NominalDiffTime -> Maybe NominalDiffTime
  f _ Nothing = Nothing
  f (tf, Sum n) (Just x) = case (realToFrac n *) <$> duration tf of
    Nothing -> Nothing
    Just y -> Just (x + y)
