module Data.Calendar (
  module Data.Timeframe,
  Event,
  event,
  eventSize,
  Calendar (..),
  singleton,
  calendar,
  insert,
  (!?),
  (!),
  Data.Calendar.toList,
  coalesce,
  totalDuration,
) where

import Data.Interval.Layers (Layers)
import Data.Interval.Layers qualified as Layers
import Data.Map.Strict qualified as Map
import Data.Time.Compat
import Data.Timeframe

-- | An 'Event' is a collection of 'Timeframe's that keeps track of
-- how deeply a particular interval has been overlapped.
--
-- > type Event n = Layers UTCTime (Sum n)
type Event n = Layers UTCTime (Sum n)

-- | Make a new 'Event' from a 'Timeframe' with default thickness 1.
--
-- > event = eventSize 1
event :: (Num n) => Timeframe -> Event n
event = (`Layers.singleton` 1)

-- | Make an 'Event' with the given size from a 'Timeframe'.
eventSize :: (Num n) => n -> Timeframe -> Event n
eventSize n = (`Layers.singleton` Sum n)

-- | A 'Calendar' is a map from a given event type to durations.
newtype Calendar ev n = Calendar {getCalendar :: Map ev (Event n)}
  deriving (Eq, Ord, Show, Typeable)

instance (Ord ev, Num n) => Semigroup (Calendar ev n) where
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord ev, Num n) => Monoid (Calendar ev n) where
  mempty = Data.Calendar.empty

-- | The empty 'Calendar'.
empty :: Calendar ev n
empty = Calendar Map.empty

-- | Make a 'Calendar' from an 'Event'.
singleton :: (Ord ev, Num n) => ev -> Event n -> Calendar ev n
singleton ev cvg = Calendar (Map.singleton ev cvg)

-- | Make a 'Calendar' from a 'Timeframe'.
calendar :: (Ord ev, Num n) => ev -> Timeframe -> Calendar ev n
calendar ev tf = singleton ev (Layers.singleton tf 1)

-- | Insert an 'Event' of the given sort into a 'Calendar'.
insert :: (Ord ev, Num n) => ev -> Event n -> Calendar ev n -> Calendar ev n
insert ev cvg (Calendar c) = Calendar (Map.insertWith (<>) ev cvg c)

-- | Get the 'Event' corresponding to a given key, or 'Nothing' if the key is not present.
(!?) :: (Ord ev, Num n) => Calendar ev n -> ev -> Maybe (Event n)
Calendar c !? ev = c Map.!? ev

-- | Get the 'Event' corresponding to a given key, or 'mempty' if the key is not present.
(!) :: (Ord ev, Num n) => Calendar ev n -> ev -> Event n
Calendar c ! ev = c Map.!? ev ?: mempty

toList :: (Ord ev, Num n) => Calendar ev n -> [(ev, [(Interval UTCTime, n)])]
toList (Calendar c) = fmap getSum <<$>> Layers.toList <<$>> Map.assocs c

-- | Consider every kind of event the same, and only observe the overall 'Layers'.
coalesce :: (Ord ev, Num n) => Calendar ev n -> Event n
coalesce (Calendar c) = fold c

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
