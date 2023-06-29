module Data.Calendar (
  module Data.Timeframe,
  Event,
  event,
  eventSize,
  erlangs,
  Calendar (..),
  singleton,
  calendar,
  insert,
  (!?),
  (!),
  Data.Calendar.toList,
  happeningAt,
  coalesce,
  totalDuration,
) where

import Control.Applicative (liftA2)
import Data.Data (Typeable)
import Data.Foldable (fold)
import Data.Interval qualified as I
import Data.Interval.Layers (Layers)
import Data.Interval.Layers qualified as Layers
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Semigroup (Sum (..))
import Data.Time.Compat (NominalDiffTime, UTCTime, diffUTCTime)
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

-- |
-- Measure the carried load of an 'Event' over a given 'Timeframe'.
-- In other words: how many copies of you would you need, in order to attend
-- all of the simultaneous happenings over a given span (on average)?
erlangs :: (Real n) => Timeframe -> Event n -> Maybe Rational
erlangs ix e =
  let diff = fmap realToFrac <$> flip diffUTCTime
   in liftA2
        (/)
        (Layers.integrate diff (realToFrac . getSum) ix e)
        (I.measuring diff ix)

-- | A 'Calendar' is a map from a given event type to durations.
newtype Calendar ev n = Calendar {getCalendar :: Map ev (Event n)}
  deriving (Eq, Ord, Show, Typeable)

instance (Ord ev, Ord n, Num n) => Semigroup (Calendar ev n) where
  (<>) ::
    (Ord ev, Ord n, Num n) => Calendar ev n -> Calendar ev n -> Calendar ev n
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord ev, Ord n, Num n) => Monoid (Calendar ev n) where
  mempty :: (Ord ev, Ord n, Num n) => Calendar ev n
  mempty = Data.Calendar.empty

-- | The empty 'Calendar'.
empty :: Calendar ev n
empty = Calendar Map.empty

-- | Make a 'Calendar' from an 'Event'.
singleton :: (Ord ev, Ord n, Num n) => ev -> Event n -> Calendar ev n
singleton ev cvg = Calendar (Map.singleton ev cvg)

-- | Make a 'Calendar' from a 'Timeframe'.
calendar :: (Ord ev, Ord n, Num n) => ev -> Timeframe -> Calendar ev n
calendar ev tf = singleton ev (Layers.singleton tf 1)

-- | Insert an 'Event' of the given sort into a 'Calendar'.
insert ::
  (Ord ev, Ord n, Num n) => ev -> Event n -> Calendar ev n -> Calendar ev n
insert ev cvg (Calendar c) = Calendar (Map.insertWith (<>) ev cvg c)

-- |
-- Get the 'Event' corresponding to a given key,
-- or 'Nothing' if the key is not present.
(!?) :: (Ord ev, Ord n, Num n) => Calendar ev n -> ev -> Maybe (Event n)
Calendar c !? ev = c Map.!? ev

-- |
-- Get the 'Event' corresponding to a given key,
-- or 'mempty' if the key is not present.
(!) :: (Ord ev, Ord n, Num n) => Calendar ev n -> ev -> Event n
Calendar c ! ev = fromMaybe mempty (c Map.!? ev)

toList ::
  (Ord ev, Ord n, Num n) => Calendar ev n -> [(ev, [(Interval UTCTime, n)])]
toList (Calendar c) = fmap (fmap (fmap getSum) . Layers.toList) <$> Map.assocs c

-- |
-- What, and how many events are happening
-- at the given 'UTCTime' on this 'Calendar'?
happeningAt :: (Ord ev, Ord n, Num n) => UTCTime -> Calendar ev n -> [(ev, n)]
happeningAt time (Data.Calendar.toList -> evs) =
  [(ev, n) | (ev, ns) <- evs, (_, n) <- filter (within time . fst) ns]

-- | Consider every kind of event the same, and observe the overall 'Layers'.
coalesce :: (Ord ev, Ord n, Num n) => Calendar ev n -> Event n
coalesce (Calendar c) = fold c

totalDuration ::
  forall ev n.
  (Ord ev, Real n) =>
  ev ->
  Calendar ev n ->
  Maybe NominalDiffTime
totalDuration ev (Calendar c) = case c Map.!? ev of
  Nothing -> Just 0
  Just is -> foldr f (Just 0) (Layers.toList is)
 where
  f :: (Timeframe, Sum n) -> Maybe NominalDiffTime -> Maybe NominalDiffTime
  f _ Nothing = Nothing
  f (tf, Sum n) (Just x) = case (realToFrac n *) <$> duration tf of
    Nothing -> Nothing
    Just y -> Just (x + y)
