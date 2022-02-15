module Data.Interval.Layers
  ( Layers,
    toLayers,
    layers,
    Data.Interval.Layers.toList,
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

import Data.IntMap.Strict qualified as IntMap
import Data.Interval (Adjacency (..), Interval)
import Data.Interval qualified as I
import Data.Interval.Covering (Covering)
import Data.Interval.Covering qualified as Covering
import Data.MultiSet (MultiSet)
import Data.MultiSet qualified as Bag
import Prelude hiding (empty)

-- The 'Layers' of an ordered type are like a 'Covering',
-- but that keeps track of how many times each point has been covered.
newtype Layers x = Layers (MultiSet (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

instance (Ord x) => Semigroup (Layers x) where
  Layers s1 <> Layers s2 =
    Layers . Bag.fromOccurList . nestings $
      Bag.toAscOccurList s1 <> Bag.toAscOccurList s2

instance (Ord x) => Monoid (Layers x) where
  mempty = Layers mempty

empty :: forall x. Layers x
empty = Layers Bag.empty

singleton :: forall x. (Ord x) => Interval x -> Layers x
singleton i = Layers (Bag.singleton i)

toLayers :: forall x. (Ord x) => [Interval x] -> Layers x
toLayers = foldMap singleton

layers :: forall x. (Ord x) => Layers x -> IntMap (Covering x)
layers (Layers s) =
  Bag.foldOccur
    (\i io -> IntMap.insertWith (<>) io (Covering.singleton i))
    mempty
    s

toList :: forall x. (Ord x) => Layers x -> [(Int, Interval x)]
toList (Layers s) = swap <$> Bag.toAscOccurList s

squash :: forall x. (Ord x) => Layers x -> Covering x
squash (Layers s) = foldMap Covering.singleton s

layersAt :: forall x. (Ord x) => x -> Layers x -> Int
layersAt x (Layers s) =
  Bag.foldOccur (\i occ acc -> if I.within x i then acc + occ else acc) 0 s

insert :: forall x. (Ord x) => Interval x -> Layers x -> Layers x
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

cutout :: (Ord x) => Interval x -> Layers x -> Layers x
cutout i (Layers s) = Layers (Bag.concatMap listDiff s)
  where
    listDiff = maybe [] Prelude.toList . (`I.difference` i)
