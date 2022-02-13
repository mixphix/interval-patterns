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

import Data.Interval (Interval)
import Data.Interval qualified as I
import Data.Interval.Adjacency
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
  (i0, io') : (j0, jo') : js -> case I.split i1 j1 of
    SomeAdjacency (adj :: Adjacent adj x) -> case adj of
      BeforeJ i j -> (i, io) : nestings ((j, jo) : js)
      MeetsJ i j k -> (i, io) : nestings ((j, io + jo) : (k, jo) : js)
      OverlapsJ i j k ->
        nestings $
          (i, io) :
          (j, io + jo) :
          (k, jo) : js
      StartsJ i j ->
        nestings $
          (i, io + jo) :
          (j, jo) : js
      DuringJ i j k ->
        nestings $
          (i, io) :
          (j, io + jo) :
          (k, jo) : js
      FinishesJ i j ->
        nestings $
          (i, io) :
          (j, io + jo) : js
      IdenticalJ i -> (i, io + jo) : nestings js
      FinishedByJ i j ->
        nestings $
          (i, io) :
          (j, io + jo) : js
      ContainsJ i j k ->
        nestings $
          (i, io) :
          (j, io + jo) :
          (k, jo) : js
      StartedByJ i j ->
        nestings $
          (i, io + jo) :
          (j, jo) : js
      OverlappedByJ i j k ->
        nestings $
          (i, io) :
          (j, io + jo) :
          (k, jo) : js
      MetByJ i j k -> (i, io) : nestings ((j, io + jo) : (k, jo) : js)
      AfterJ i j -> (i, io) : nestings ((j, jo) : js)
    where
      ((i1, io), (j1, jo))
        | i0 <= j0 = ((i0, io'), (j0, jo'))
        | otherwise = ((j0, jo'), (i0, io'))
  x -> x

cutout :: (Ord x) => Interval x -> IntervalLayer x -> IntervalLayer x
cutout i (IntervalLayer s) = IntervalLayer (Bag.concatMap listDiff s)
  where
    listDiff = maybe [] toList . (`I.difference` i)

-- nestingAt :: (Ord x) => x -> IntervalLayer x -> Int
-- nestingAt x (IntervalLayer m) =
--   let m' = Map.mapMaybeWithKey (\i n -> if I.within x i then Just n else Nothing) m
--    in maybe 0 snd $ Map.lookupMax m'

-- nestings :: (Ord a, Show a) => Overlap a -> Map Int (Cover a)
-- nestings (Overlap os) =
--   let bounds = foldr (\(Interval l u) -> (IsLowerBound l :) . (IsUpperBound u :)) [] os
--       covered = go 0 (sort bounds) Map.empty
--    in Map.insert 0 (C.complement $ foldMap cover os) covered
--   where
--     go n (IsLowerBound l : x@(IsLowerBound (LowerBound ll bll)) : rest) m =
--       go (n + 1) (x : rest) $
--         Map.insertWith
--           (<>)
--           (n + 1)
--           (cover $ Interval l (UpperBound ll (I.flipBoundary bll)))
--           m
--     go n (IsLowerBound l : x@(IsUpperBound u) : rest) m =
--       go (n + 1) (x : rest) $
--         Map.insertWith
--           (<>)
--           (n + 1)
--           (cover $ Interval l u)
--           m
--     go n (IsUpperBound (UpperBound u bu) : x@(IsUpperBound (UpperBound uu buu)) : rest) m =
--       go (n - 1) (x : rest) $
--         Map.insertWith
--           (<>)
--           (n - 1)
--           (cover $ Interval (LowerBound u (I.flipBoundary bu)) (UpperBound uu buu))
--           m
--     go _ _ m = m
