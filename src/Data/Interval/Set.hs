module Data.Interval.Set
  ( IntervalSet,
    intervalSet,
    Data.Interval.Set.empty,
    singleton,
    Data.Interval.Set.null,
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

import Data.Interval qualified as I
import Data.Interval.Types (Interval)
import Data.OneOrTwo (OneOrTwo (..))
import Data.Set qualified as Set

-- | An 'IntervalSet' is an ordered list of intervals
-- such that overlapping intervals are automatically merged.
--
-- It is a monoid that is convenient for agglomerating
-- groups of intervals, such as for calculating the overall timespan
-- of a group of events. However, it is agnostic of
-- how many times each given point has been covered.
newtype IntervalSet x = IntervalSet (Set (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

-- | Turn an 'IntervalSet' into a 'Set.Set' of 'Interval's.
intervalSet :: (Ord x) => IntervalSet x -> Set (Interval x)
intervalSet (IntervalSet is) = unionsSet is

unionsSet :: (Ord x) => Set (Interval x) -> Set (Interval x)
unionsSet = Set.fromList . I.unions . Set.toList

-- | The empty 'IntervalSet'.
empty :: (Ord x) => IntervalSet x
empty = IntervalSet Set.empty

-- | The 'IntervalSet' consisting of a single 'Interval'.
singleton :: (Ord x) => Interval x -> IntervalSet x
singleton x = IntervalSet (Set.singleton x)

-- | Is this 'IntervalSet' empty?
null :: IntervalSet x -> Bool
null (IntervalSet is) = Set.null is

-- | Turn a list of 'Interval's into an 'IntervalSet'.
intervals :: (Ord x) => [Interval x] -> IntervalSet x
intervals = IntervalSet . Set.fromList . I.unions

-- | Insert an 'Interval' into an 'IntervalSet', agglomerating along the way.
insert :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
insert i (IntervalSet is) = IntervalSet (unionsSet (Set.insert i is))

-- | The 'IntervalSet' that covers the entire range.
whole :: (Ord x) => IntervalSet x
whole = IntervalSet (one I.Whole)

instance (Ord x) => Semigroup (IntervalSet x) where
  IntervalSet is <> IntervalSet js = IntervalSet (unionsSet (is <> js))

instance (Ord x) => Monoid (IntervalSet x) where mempty = IntervalSet mempty

instance (Ord x) => One (IntervalSet x) where
  type OneItem _ = Interval x
  one = singleton

-- | Completely remove an 'Interval' from an 'IntervalSet'.
cutout :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
cutout i (IntervalSet is) =
  intervals . (`foldMap` is) $
    (`I.difference` i) >>> \case
      Nothing -> mempty
      Just (One j) -> [j]
      Just (Two j k) -> [j, k]

-- | Is this point 'I.within' any 'Interval' in the 'IntervalSet'?
member :: (Ord x) => x -> IntervalSet x -> Bool
member x (IntervalSet is) = any (I.within x) is

-- | Is this point not 'I.within' any 'Interval' in the 'IntervalSet'?
notMember :: (Ord x) => x -> IntervalSet x -> Bool
notMember x = not . member x

-- | A synonym for '(<>)'.
union :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
union = (<>)

-- | A synonym for 'fold'.
unions :: (Ord x) => [IntervalSet x] -> IntervalSet x
unions = fold

-- | Remove all intervals of the second set from the first.
difference :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
difference is (IntervalSet js) = foldr cutout is js

-- | Take the 'IntervalSet' consisting of each point not in the given one.
complement :: (Ord x) => IntervalSet x -> IntervalSet x
complement = difference whole

-- | Given an 'Interval' @i@, @'clip' i@ will trim an 'IntervalSet'
-- so that its hull is contained in @i@.
clip :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
clip i (IntervalSet js) =
  foldr ((<>) . maybe mempty one . I.intersect i) mempty js

-- | Take the intersection of two 'IntervalSet's.
intersection :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
intersection is (IntervalSet js) = foldMap (`clip` is) js

-- | Take the intersection of a non-empty list of 'IntervalSet's.
intersections :: (Ord x) => NonEmpty (IntervalSet x) -> IntervalSet x
intersections (i :| []) = i
intersections (i :| j : js) = intersections $ intersection i j :| js

-- | Take the smallest spanning 'Interval' of an 'IntervalSet',
-- provided that it is not the empty set.
hull :: (Ord x) => IntervalSet x -> Maybe (Interval x)
hull (IntervalSet is)
  | Set.null is = Nothing
  | otherwise = Just $ uncurry (foldr I.hull) (Set.deleteFindMin is)
