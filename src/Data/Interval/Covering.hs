module Data.Interval.Covering
  ( Covering,
    intervalSet,
    Data.Interval.Covering.empty,
    singleton,
    Data.Interval.Covering.null,
    intervals,
    insert,
    whole,
    cutout,
    member,
    notMember,
    union,
    unions,
    difference,
    complement,
    intersection,
    intersections,
    hull,
  )
where

import Data.Interval (Interval)
import Data.Interval qualified as I
import Data.OneOrTwo (OneOrTwo (..))
import Data.Set qualified as Set

-- | A 'Covering' is an ordered list of intervals
-- such that overlapping intervals are automatically merged.
--
-- It is a monoid that is convenient for agglomerating
-- groups of intervals, such as for calculating the overall timespan
-- of a group of events. However, it is agnostic of
-- how many times each given point has been covered.
-- To keep track of this data, use 'Data.Interval.Layers'.
newtype Covering x = Covering (Set (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

-- | Turn an 'Covering' into a 'Set.Set' of 'Interval's.
intervalSet :: (Ord x) => Covering x -> Set (Interval x)
intervalSet (Covering is) = unionsSet is

unionsSet :: (Ord x) => Set (Interval x) -> Set (Interval x)
unionsSet = Set.fromList . I.unions . Set.toList

-- | The empty 'Covering'.
empty :: (Ord x) => Covering x
empty = Covering Set.empty

-- | The 'Covering' consisting of a single 'Interval'.
singleton :: (Ord x) => Interval x -> Covering x
singleton x = Covering (Set.singleton x)

-- | Is this 'Covering' empty?
null :: Covering x -> Bool
null (Covering is) = Set.null is

-- | Turn a list of 'Interval's into an 'Covering'.
intervals :: (Ord x) => [Interval x] -> Covering x
intervals = Covering . Set.fromList . I.unions

-- | Insert an 'Interval' into an 'Covering', agglomerating along the way.
insert :: (Ord x) => Interval x -> Covering x -> Covering x
insert i (Covering is) = Covering (unionsSet (Set.insert i is))

-- | The 'Covering' that covers the entire range.
whole :: (Ord x) => Covering x
whole = Covering (one I.Whole)

instance (Ord x) => Semigroup (Covering x) where
  Covering is <> Covering js = Covering (unionsSet (is <> js))

instance (Ord x) => Monoid (Covering x) where mempty = Covering mempty

instance (Ord x) => One (Covering x) where
  type OneItem _ = Interval x
  one = singleton

-- | Completely remove an 'Interval' from an 'Covering'.
cutout :: (Ord x) => Interval x -> Covering x -> Covering x
cutout i (Covering is) =
  intervals . (`foldMap` is) $
    (`I.difference` i) >>> \case
      Nothing -> mempty
      Just (One j) -> [j]
      Just (Two j k) -> [j, k]

-- | Is this point 'I.within' any 'Interval' in the 'Covering'?
member :: (Ord x) => x -> Covering x -> Bool
member x (Covering is) = any (I.within x) is

-- | Is this point not 'I.within' any 'Interval' in the 'Covering'?
notMember :: (Ord x) => x -> Covering x -> Bool
notMember x = not . member x

-- | A synonym for '(<>)'.
union :: (Ord x) => Covering x -> Covering x -> Covering x
union = (<>)

-- | A synonym for 'fold'.
unions :: (Ord x) => [Covering x] -> Covering x
unions = fold

-- | Remove all intervals of the second set from the first.
difference :: (Ord x) => Covering x -> Covering x -> Covering x
difference is (Covering js) = foldr cutout is js

-- | Take the 'Covering' consisting of each point not in the given one.
complement :: (Ord x) => Covering x -> Covering x
complement = difference whole

-- | Given an 'Interval' @i@, @'clip' i@ will trim an 'Covering'
-- so that its hull is contained in @i@.
clip :: (Ord x) => Interval x -> Covering x -> Covering x
clip i (Covering js) =
  foldr ((<>) . maybe mempty one . I.intersect i) mempty js

-- | Take the intersection of two 'Covering's.
intersection :: (Ord x) => Covering x -> Covering x -> Covering x
intersection is (Covering js) = foldMap (`clip` is) js

-- | Take the intersection of a non-empty list of 'Covering's.
intersections :: (Ord x) => NonEmpty (Covering x) -> Covering x
intersections (i :| []) = i
intersections (i :| j : js) = intersections $ intersection i j :| js

-- | Take the smallest spanning 'Interval' of an 'Covering',
-- provided that it is not the empty set.
hull :: (Ord x) => Covering x -> Maybe (Interval x)
hull (Covering is)
  | Set.null is = Nothing
  | otherwise = Just $ uncurry (foldr I.hull) (Set.deleteFindMin is)
