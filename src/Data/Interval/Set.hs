module Data.Interval.Set where

import Data.Interval (Interval)
import Data.Interval qualified as I
import Data.OneOrTwo (OneOrTwo (..))
import Data.Set qualified as Set

newtype IntervalSet x = IntervalSet (Set (Interval x))
  deriving (Eq, Ord, Show, Generic, Typeable)

intervalSet :: (Ord x) => IntervalSet x -> Set (Interval x)
intervalSet (IntervalSet is) = Set.map I.orient (unionsSet is)

unionsSet :: (Ord x) => Set (Interval x) -> Set (Interval x)
unionsSet = Set.fromList . I.unions . Set.toList

singleton :: (Ord x) => Interval x -> IntervalSet x
singleton x = IntervalSet (Set.singleton (I.orient x))

intervals :: (Ord x) => [Interval x] -> IntervalSet x
intervals = IntervalSet . Set.fromList . I.unions

insert :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
insert i (IntervalSet is) = IntervalSet (unionsSet (Set.insert i is))

whole :: IntervalSet x
whole = IntervalSet (one I.Whole)

instance (Ord x) => Semigroup (IntervalSet x) where
  IntervalSet is <> IntervalSet js = IntervalSet (unionsSet (is <> js))

instance (Ord x) => Monoid (IntervalSet x) where mempty = IntervalSet mempty

instance (Ord x) => One (IntervalSet x) where
  type OneItem _ = Interval x
  one = singleton

cutout :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
cutout i (IntervalSet is) =
  intervals . (`foldMap` is) $
    (`I.difference` i) >>> \case
      Nothing -> mempty
      Just (One j) -> [j]
      Just (Two j k) -> [j, k]

member :: (Ord x) => x -> IntervalSet x -> Bool
member x (IntervalSet is) = any (I.within x) is

notMember :: (Ord x) => x -> IntervalSet x -> Bool
notMember x = not . member x

union :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
union = (<>)

difference :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
difference is (IntervalSet js) = foldr cutout is js

complement :: (Ord x) => IntervalSet x -> IntervalSet x
complement = difference whole

clip :: (Ord x) => Interval x -> IntervalSet x -> IntervalSet x
clip i (IntervalSet js) =
  foldr ((<>) . maybe mempty one . I.intersect i) mempty js

intersection :: (Ord x) => IntervalSet x -> IntervalSet x -> IntervalSet x
intersection is (IntervalSet js) = foldMap (`clip` is) js

hull :: (Ord x) => IntervalSet x -> Maybe (Interval x)
hull (IntervalSet is)
  | null is = Nothing
  | otherwise = Just $ uncurry (foldr I.hull) (Set.deleteFindMin is)
