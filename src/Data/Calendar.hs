module Data.Calendar (
  module Data.Timeframe,
  Event,
  event,
  eventSize,
  erlangs,
  Calendar (..),
  empty,
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
import Control.Block
import Data.Eq
import Data.Foldable (fold, foldr)
import Data.Function
import Data.Interval qualified as I
import Data.Interval.Layers (Layers)
import Data.Interval.Layers qualified as Layers
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid (Monoid (..))
import Data.Ord
import Data.Ratio (Rational)
import Data.Semigroup (Semigroup ((<>)))
import Data.Time
import Data.Timeframe
import Data.Tuple
import GHC.Show (Show)
import Numeric.Natural (Natural)

import Bolt.Math

-- | An 'Event' is a collection of 'Timeframe's that keeps track of
-- how deeply a particular interval has been overlapped.
--
-- > type Event = Layers UTCTime (Sum n)
type Event = Layers UTCTime Natural

-- | Make a new 'Event' from a 'Timeframe' with default thickness 1.
--
-- > event = eventSize 1
event :: Timeframe -> Event
event = (`Layers.singleton` fromNatural 1)

-- | Make an 'Event' with the given size from a 'Timeframe'.
eventSize :: Natural -> Timeframe -> Event
eventSize = flip Layers.singleton

-- |
-- Measure the carried load of an 'Event' over a given 'Timeframe'.
-- In other words: how many copies of you would you need, in order to attend
-- all of the simultaneous happenings over a given span (on average)?
erlangs :: Timeframe -> Event -> Maybe Rational
erlangs ix e =
  let diff = fmap (\(Nominal (Picoseconds x)) -> x) <$> flip diffUTCTime
   in liftA2
        (/.)
        (Layers.area diff toInteger ix e)
        (I.measuring diff ix)

-- | A 'Calendar' is a map from a given event type to durations.
newtype Calendar ev = Calendar {getCalendar :: Map ev Event}
  deriving (Eq, Ord, Show)

instance (Ord ev) => Semigroup (Calendar ev) where
  (<>) :: Calendar ev -> Calendar ev -> Calendar ev
  Calendar a <> Calendar b = Calendar (Map.unionWith (<>) a b)

instance (Ord ev) => Monoid (Calendar ev) where
  mempty :: (Ord ev) => Calendar ev
  mempty = Calendar Map.empty

-- | The empty 'Calendar'.
empty :: Calendar ev
empty = Calendar Map.empty

-- | Make a 'Calendar' from an 'Event'.
singleton :: (Ord ev) => ev -> Event -> Calendar ev
singleton ev cvg = Calendar (Map.singleton ev cvg)

-- | Make a 'Calendar' from a 'Timeframe'.
calendar :: (Ord ev) => ev -> Timeframe -> Calendar ev
calendar ev tf = singleton ev (Layers.singleton tf (fromNatural 1))

-- | Insert an 'Event' of the given sort into a 'Calendar'.
insert ::
  (Ord ev) => ev -> Event -> Calendar ev -> Calendar ev
insert ev cvg (Calendar c) = Calendar (Map.insertWith (<>) ev cvg c)

-- |
-- Get the 'Event' corresponding to a given key,
-- or 'Nothing' if the key is not present.
(!?) :: (Ord ev) => Calendar ev -> ev -> Maybe Event
Calendar c !? ev = c Map.!? ev

-- |
-- Get the 'Event' corresponding to a given key,
-- or 'mempty' if the key is not present.
(!) :: (Ord ev) => Calendar ev -> ev -> Event
Calendar c ! ev = fromMaybe mempty (c Map.!? ev)

toList ::
  (Ord ev) => Calendar ev -> [(ev, [(Interval UTCTime, Natural)])]
toList (Calendar c) = fmap Layers.toList <$> Map.assocs c

-- |
-- What, and how many events are happening
-- at the given 'UTCTime' on this 'Calendar'?
happeningAt :: (Ord ev) => UTCTime -> Calendar ev -> [(ev, Natural)]
happeningAt time (Data.Calendar.toList -> evs) =
  [ (ev, n)
  | (ev, ns) <- evs
  , (_, n) <- filter (within (Meridian time) . fst) ns
  ]

-- | Consider every kind of event the same, and observe the overall 'Layers'.
coalesce :: (Ord ev) => Calendar ev -> Event
coalesce (Calendar c) = fold c

-- | Calculate the total length of a particular event across all occurrences.
totalDuration ::
  forall ev.
  (Ord ev) =>
  ev ->
  Calendar ev ->
  Maybe NominalDiffTime
totalDuration ev (Calendar c) = case c Map.!? ev of
  Nothing -> Just 0
  Just is -> foldr f (Just 0) (Layers.toList is)
 where
  f :: (Timeframe, Natural) -> Maybe NominalDiffTime -> Maybe NominalDiffTime
  f _ Nothing = Nothing
  f (tf, n) (Just x) = case (toInteger n *.) <$> duration tf of
    Nothing -> Nothing
    Just (y :: NominalDiffTime) -> Just (x + y)
