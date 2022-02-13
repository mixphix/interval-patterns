module Data.Interval.Layered
  ( IntervalLayer,
    toLayers,
    layers,
    empty,
    singleton,
    insert,
    squash,
    layersAt,
    cutout,

    -- ** Helper functions
    nestings,
  )
where

import Data.Interval (Adjacency (..), Interval)
import Data.Interval qualified as I
import Data.Interval.Set (IntervalSet)
import Data.Interval.Set qualified as IS
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as Bag
import Prelude hiding (empty)

-- The 'IntervalLayer's of an ordered type are like an 'IntervalSet',
-- but that keeps track of how many times each point has been covered.
newtype IntervalLayer x = IntervalLayer (MultiSet (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

instance (Ord x) => Semigroup (IntervalLayer x) where
  IntervalLayer s1 <> IntervalLayer s2 =
    IntervalLayer . Bag.fromOccurList . nestings $
      Bag.toAscOccurList s1 <> Bag.toAscOccurList s2

instance (Ord x) => Monoid (IntervalLayer x) where
  mempty = IntervalLayer mempty

empty :: forall x. IntervalLayer x
empty = IntervalLayer Bag.empty

singleton :: forall x. (Ord x) => Interval x -> IntervalLayer x
singleton i = IntervalLayer (Bag.singleton i)

toLayers :: forall x. (Ord x) => [Interval x] -> IntervalLayer x
toLayers = foldMap singleton

layers :: forall x. (Ord x) => IntervalLayer x -> [Interval x]
layers (IntervalLayer s) = toList s

squash :: forall x. (Ord x) => IntervalLayer x -> IntervalSet x
squash (IntervalLayer s) = foldMap IS.singleton s

layersAt :: forall x. (Ord x) => x -> IntervalLayer x -> Int
layersAt x (IntervalLayer s) =
  Bag.foldOccur (\i occ acc -> if I.within x i then acc + occ else acc) 0 s

insert :: forall x. (Ord x) => Interval x -> IntervalLayer x -> IntervalLayer x
insert = mappend . singleton

nestings ::
  forall x.
  (Ord x) =>
  [(Interval x, Int)] ->
  [(Interval x, Int)]
nestings = \case
  (x, xo) : (y, yo) : js ->
    let ((i', io), (j', jo))
          | x <= y = ((x, xo), (y, yo))
          | otherwise = ((y, yo), (x, xo))
     in case I.adjacency i' j' of
          Before i j -> (i, io) : nestings ((j, jo) : js)
          Meets i j k -> (i, io) : nestings ((j, io + jo) : (k, jo) : js)
          Overlaps i j k ->
            nestings $
              (i, io) :
              (j, io + jo) :
              (k, jo) : js
          Starts i j ->
            nestings $
              (i, io + jo) :
              (j, jo) : js
          During i j k ->
            nestings $
              (i, io) :
              (j, io + jo) :
              (k, jo) : js
          Finishes i j ->
            nestings $
              (i, io) :
              (j, io + jo) : js
          Identical i -> (i, io + jo) : nestings js
          FinishedBy i j ->
            nestings $
              (i, io) :
              (j, io + jo) : js
          Contains i j k ->
            nestings $
              (i, io) :
              (j, io + jo) :
              (k, jo) : js
          StartedBy i j ->
            nestings $
              (i, io + jo) :
              (j, jo) : js
          OverlappedBy i j k ->
            nestings $
              (i, io) :
              (j, io + jo) :
              (k, jo) : js
          MetBy i j k -> (i, io) : nestings ((j, io + jo) : (k, jo) : js)
          After i j -> (i, io) : nestings ((j, jo) : js)
  x -> x

cutout :: (Ord x) => Interval x -> IntervalLayer x -> IntervalLayer x
cutout i (IntervalLayer s) = IntervalLayer (Bag.concatMap listDiff s)
  where
    listDiff = maybe [] toList . (`I.difference` i)
