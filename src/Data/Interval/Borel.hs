module Data.Interval.Borel
  ( Borel,
    covering,
    intervalSet,
    Data.Interval.Borel.empty,
    singleton,
    Data.Interval.Borel.null,
    insert,
    whole,
    cutout,
    member,
    notMember,
    union,
    unions,
    difference,
    symmetricDifference,
    complement,
    intersection,
    intersections,
    hull,
  )
where

import Algebra.Heyting
import Algebra.Lattice
import Data.Interval (Interval)
import Data.Interval qualified as I
import Data.OneOrTwo (OneOrTwo (..))
import Data.Semiring (Ring, Semiring)
import Data.Semiring qualified as Semiring
import Data.Set qualified as Set
import Prelude

-- | A 'Borel' is an ordered list of intervals
-- such that overlapping intervals are automatically merged.
--
-- It is a monoid that is convenient for agglomerating
-- groups of intervals, such as for calculating the overall timespan
-- of a group of events. However, it is agnostic of
-- how many times each given point has been covered.
-- To keep track of this data, use 'Data.Interval.Layers'.
newtype Borel x = Borel (Set (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

instance (Ord x, Lattice x) => Lattice (Borel x) where
  (\/) = union
  (/\) = intersection

instance (Ord x, Lattice x) => BoundedMeetSemiLattice (Borel x) where
  top = whole

instance (Ord x, Lattice x) => BoundedJoinSemiLattice (Borel x) where
  bottom = mempty

instance (Ord x, Lattice x) => Heyting (Borel x) where
  x ==> y = complement x \/ y

instance (Ord x, Lattice x) => Semiring (Borel x) where
  plus = symmetricDifference
  times = intersection
  zero = mempty
  one = whole

instance (Ord x, Lattice x) => Ring (Borel x) where
  negate = complement

-- | Turn a list of 'Interval's into an 'Borel'.
covering :: (Ord x) => [Interval x] -> Borel x
covering = Borel . Set.fromList . I.unions

-- | Turn an 'Borel' into a 'Set.Set' of 'Interval's.
intervalSet :: (Ord x) => Borel x -> Set (Interval x)
intervalSet (Borel is) = unionsSet is

unionsSet :: (Ord x) => Set (Interval x) -> Set (Interval x)
unionsSet = Set.fromAscList . I.unionsAsc . Set.toAscList

-- | The empty 'Borel'.
empty :: (Ord x) => Borel x
empty = Borel Set.empty

-- | The 'Borel' consisting of a single 'Interval'.
singleton :: (Ord x) => Interval x -> Borel x
singleton x = Borel (Set.singleton x)

-- | Is this 'Borel' empty?
null :: Borel x -> Bool
null (Borel is) = Set.null is

-- | Insert an 'Interval' into an 'Borel', agglomerating along the way.
insert :: (Ord x) => Interval x -> Borel x -> Borel x
insert i (Borel is) = Borel (unionsSet (Set.insert i is))

-- | The 'Borel' that covers the entire range.
whole :: (Ord x) => Borel x
whole = Borel (Prelude.one I.Whole)

instance (Ord x) => Semigroup (Borel x) where
  Borel is <> Borel js = Borel (unionsSet (is <> js))

instance (Ord x) => Monoid (Borel x) where mempty = Borel mempty

instance (Ord x) => One (Borel x) where
  type OneItem _ = Interval x
  one = singleton

-- | Completely remove an 'Interval' from an 'Borel'.
cutout :: (Ord x) => Interval x -> Borel x -> Borel x
cutout i (Borel is) =
  covering . flip foldMap is $
    (I.\\ i) >>> \case
      Nothing -> mempty
      Just (One j) -> [j]
      Just (Two j k) -> [j, k]

-- | Is this point 'I.within' any 'Interval' in the 'Borel'?
member :: (Ord x) => x -> Borel x -> Bool
member x (Borel is) = any (I.within x) is

-- | Is this point not 'I.within' any 'Interval' in the 'Borel'?
notMember :: (Ord x) => x -> Borel x -> Bool
notMember x = not . member x

-- | A synonym for '(<>)'.
union :: (Ord x) => Borel x -> Borel x -> Borel x
union = (<>)

-- | A synonym for 'fold'.
unions :: (Ord x) => [Borel x] -> Borel x
unions = fold

-- | Remove all intervals of the second set from the first.
difference :: (Ord x) => Borel x -> Borel x -> Borel x
difference is (Borel js) = foldr cutout is js

symmetricDifference :: (Ord x) => Borel x -> Borel x -> Borel x
symmetricDifference is js = difference is js <> difference js is

-- | Take the 'Borel' consisting of each point not in the given one.
complement :: (Ord x) => Borel x -> Borel x
complement = difference whole

-- | Given an 'Interval' @i@, @'clip' i@ will trim an 'Borel'
-- so that its hull is contained in @i@.
clip :: (Ord x) => Interval x -> Borel x -> Borel x
clip i (Borel js) =
  foldr ((<>) . maybe mempty one . I.intersect i) mempty js

-- | Take the intersection of two 'Borel's.
intersection :: (Ord x) => Borel x -> Borel x -> Borel x
intersection is (Borel js) = foldMap (`clip` is) js

-- | Take the intersection of a non-empty list of 'Borel's.
intersections :: (Ord x) => NonEmpty (Borel x) -> Borel x
intersections (i :| []) = i
intersections (i :| j : js) = intersections $ intersection i j :| js

-- | Take the smallest spanning 'Interval' of an 'Borel',
-- provided that it is not the empty set.
hull :: (Ord x) => Borel x -> Maybe (Interval x)
hull (Borel is)
  | Set.null is = Nothing
  | otherwise = Just $ uncurry (foldr I.hull) (Set.deleteFindMin is)
