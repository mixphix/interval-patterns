module Data.Interval.Layers (
  Layers (Layers),
  Data.Interval.Layers.fromList,
  Data.Interval.Layers.toList,
  empty,
  singleton,
  shadow,
  shadowing,
  ishadowing,
  land,
  landAbove,
  height,
  tallest,
  remove,
  (\-),
  difference,
  trim,
  (\=),
  toStepFunction,
  area,
  areaRing,

  -- ** Helper functions
  nestings,
) where

import Data.Bool
import Data.Data (Data)
import Data.Eq
import Data.Foldable qualified as Data
import Data.Function (const, flip, ($))
import Data.Functor qualified as Data
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.Interval (
  Adjacency (..),
  Interval,
  OneOrTwo (..),
  data (:---:),
  data (:|-|:),
 )
import Data.Interval qualified as Interval
import Data.Interval.Borel (Borel)
import Data.Interval.Borel qualified as Borel
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Semigroup
import Data.Traversable qualified as Data
import Data.Tuple
import GHC.Generics (Generic)
import GHC.Show (Show)

import Flex.Math
import Flex.Math.Category

-- The 'Layers' of an ordered type @x@ are like the 'Borel' sets,
-- but that keeps track of how far each point has been "raised" in @y@.
newtype Layers x y = Layers (Map (Interval x) y)
  deriving (Eq, Ord, Show, Data.Functor, Data.Foldable, Data.Traversable, Generic, Data)

instance Morphisms (->) (->) (Layers x) where
  morphism :: (z -> y) -> Layers x z -> Layers x y
  morphism z_y (Layers m) = Layers (morphism z_y m)
instance Morphisms (Ix (Interval x)) (->) (Layers x) where
  morphism :: Ix (Interval x) z y -> Layers x z -> Layers x y
  morphism (Ix i_z_y) (Layers m) = Layers (morphism (Ix i_z_y) m)
instance Folds (->) (->) (Layers x) where
  foldWith :: (Monoid z) => (y -> z) -> Layers x y -> z
  foldWith y_z (Layers m) = foldWith y_z m
instance Folds (Ix (Interval x)) (->) (Layers x) where
  foldWith :: (Monoid z) => Ix (Interval x) y z -> Layers x y -> z
  foldWith (Ix i_y_z) (Layers m) = foldWith (Ix i_y_z) m
instance Traversals (->) (->) (Layers x) where
  traverse :: (Applicative g) => (z -> g y) -> Layers x z -> g (Layers x y)
  traverse z_gy (Layers m) = morphism Layers (traverse z_gy m)
instance Traversals (Ix (Interval x)) (->) (Layers x) where
  traverse :: (Applicative g) => Ix (Interval x) z (g y) -> Layers x z -> g (Layers x y)
  traverse (Ix i_z_gy) (Layers m) = morphism Layers (traverse (Ix i_z_gy) m)

instance (Ord x, Ord y, Additive y) => Semigroup (Layers x y) where
  (<>) :: Layers x y -> Layers x y -> Layers x y
  Layers s1 <> Layers s2 =
    Layers
      . Map.fromAscList
      . nestingsAsc
      . Heap.fromList
      $ Map.toAscList (Map.unionWith (+) s1 s2)

instance (Ord x, Ord y, Additive y) => Monoid (Layers x y) where
  mempty :: Layers x y
  mempty = empty

instance (Ord x, Ord y, Additive y) => Addition (Layers x y) (Layers x y) (Layers x y) where
  (+.) :: Layers x y -> Layers x y -> Layers x y
  (+.) = (<>)

instance (Ord x, Ord y, Additive y) => Additive (Layers x y) where
  zero :: Layers x y
  zero = mempty

instance (Ord x, Ord y, AdditiveGroup y) => Subtraction (Layers x y) (Layers x y) (Layers x y) where
  (-.) :: Layers x y -> Layers x y -> Layers x y
  s -. t = s + Data.fmap negative t

instance (Ord x, Ord y, AdditiveGroup y) => AdditiveGroup (Layers x y) where
  negative :: Layers x y -> Layers x y
  negative (Layers s) = Layers (Data.fmap negative s)

instance (Ord x, Ord y, Additive y) => Addition (Interval x, y) (Layers x y) (Layers x y) where
  (+.) :: (Interval x, y) -> Layers x y -> Layers x y
  (+.) = (+) . uncurry singleton

instance (Ord x, Ord y, Additive y) => Addition (Layers x y) (Interval x, y) (Layers x y) where
  (+.) :: Layers x y -> (Interval x, y) -> Layers x y
  (+.) = flip (+.)

instance (Ord x, Ord y, AdditiveGroup y) => Subtraction (Layers x y) (Interval x, y) (Layers x y) where
  (-.) :: Layers x y -> (Interval x, y) -> Layers x y
  l -. (ix, y) = l - singleton ix y

-- | A blank canvas.
empty :: Layers x y
empty = Layers Map.empty

-- | @singleton ix y@ is the rectangle with base @ix@ of height @y@.
singleton :: (Ord x) => Interval x -> y -> Layers x y
singleton ix y = Layers (Map.singleton ix y)

-- | Draw the 'Layers' of specified supports and heights.
fromList :: (Ord x, Ord y, Additive y) => [(Interval x, y)] -> Layers x y
fromList = Layers . Map.fromList . nestings

-- | Get all of the supports and heights in the 'Layers'.
toList :: (Ord x) => Layers x y -> [(Interval x, y)]
toList (Layers s) = Map.toList s

-- | Ignore the 'Layers' and focus only on whether points are 'Data.Interval.within'
-- any contained 'Interval' or not.
shadow :: (Ord x) => Layers x y -> Borel x
shadow (Layers s) = foldWith Borel.singleton (Map.keys s)

-- | 'shadow' together the intervals satisfying a predicate.
shadowing :: (Ord x) => (y -> Bool) -> Layers x y -> Borel x
shadowing = ishadowing . const

-- | Perform 'shadowing' with a test that accepts the 'Interval' as an argument.
ishadowing :: (Ord x) => (Interval x -> y -> Bool) -> Layers x y -> Borel x
ishadowing f s = flip ifoldWith s \ix y -> if f ix y then Borel.singleton ix else Borel.empty

-- | Treating 'zero' as sea level, consider the 'Borel' set of a provided
-- 'Layers' that is "land".
land :: (Ord x, Ord y, Additive y) => Layers x y -> Borel x
land = landAbove zero

-- | Given a "sea level", consider the 'Borel' set of a provided 'Layers'
-- that is "land".
landAbove :: (Ord x, Ord y) => y -> Layers x y -> Borel x
landAbove sea (Layers s) = flip Map.foldMapWithKey s \i y ->
  if y > sea then Borel.singleton i else Borel.empty

-- | Completely eliminate an 'Interval' from the support of the 'Layers'.
remove :: (Ord x, Ord y, Additive y) => Interval x -> Layers x y -> Layers x y
remove ix (Layers s) = flip (`Map.foldlWithKey'` empty) s \acc jx y ->
  acc <> case jx Interval.\\ ix of
    Nothing -> mempty
    Just (One kx) -> singleton kx y
    Just (Two kx lx) -> fromList [(kx, y), (lx, y)]

-- | Fliped infix version of 'remove'.
(\-) :: (Ord x, Ord y, Additive y) => Layers x y -> Interval x -> Layers x y
(\-) = flip remove

-- | Pointwise subtraction of the second argument from the first.
difference :: (Ord x, Ord y, Semigroup y, AdditiveGroup y) => Layers x y -> Layers x y -> Layers x y
difference layers (Layers s) =
  foldr (flip (-.)) layers (Map.toAscList s)

-- | Restrict the support of the 'Layers' to the given 'Interval'.
trim ::
  (Ord x, Ord y, Additive y) => Interval x -> Layers x y -> Layers x y
trim ix (Layers s) =
  flip (`Map.foldlWithKey'` empty) s \acc jx y ->
    maybe id ((+.) . (,y)) (Interval.intersect ix jx) acc

-- | Flipped infix version of 'trim'.
(\=) :: (Ord x, Ord y, Additive y) => Layers x y -> Interval x -> Layers x y
(\=) = flip trim

-- |
-- @'integrate' zwidth zheight ix l@ calculates the area under the 'Interval' @ix@
-- using the @zwidth@ of the interval multiplied by the @zheight@
-- of the layers over each sub-interval in the layers.
area ::
  forall x y z.
  (Ord x, Ord y, Additive y, Semiring z) =>
  (x -> x -> z) ->
  (y -> z) ->
  Interval x ->
  Layers x y ->
  Maybe z
area zwidth zheight support l =
  let Layers (Map.assocs -> supp) = l \= support
      f accumulator (ix, y) = do
        acc <- accumulator
        zdeltaX <- Interval.measuring zwidth ix
        pure $ acc + zdeltaX * zheight y
   in foldl f (Just zero) supp

areaRing :: (Ord x, Ring x) => Interval x -> Layers x x -> Maybe x
areaRing = area (flip (-)) id

-- | Get the height of the 'Layers' at a point.
height :: (Ord x, Semigroup y) => Suspension x -> Layers x y -> Maybe y
height x (Layers s) = case Map.lookupLE (x :|-|: x) s of
  Just (ix, y) | x `Interval.within` ix -> Just y
  _ -> Nothing

-- | Where and how tall is the tallest 'Interval'?
tallest :: (Ord x, Ord y) => Layers x y -> Maybe (Interval x, y)
tallest (Layers s) =
  flip (`Map.foldlWithKey'` Nothing) s \acc ix y -> Just case acc of
    Nothing -> (ix, y)
    Just (_, y') | y > y' -> (ix, y)
    Just (ix', y') -> (ix', y')

-- | Convert the 'Layers' into a list of beginning-points and heights,
-- that define a step function piecewise.
toStepFunction :: (Ord x, Ord y, Monoid y) => Layers x y -> [(Suspension x, y)]
toStepFunction = go . Data.Interval.Layers.toList
 where
  go = \case
    [(il :---: iu, iy), (j@(jl :---: North), jy)]
      | iu == jl -> (il, iy) : go [(j, jy)]
      | otherwise -> (il, iy) : (iu, mempty) : go [(j, jy)]
    (il :---: iu, iy) : (j@(jl :---: _), jy) : is
      | iu == jl -> (il, iy) : go ((j, jy) : is)
      | otherwise -> (il, iy) : (iu, mempty) : go ((j, jy) : is)
    [(il :---: iu, iy)] -> [(il, iy), (iu, mempty)]
    [] -> []

nestings ::
  (Ord x, Ord y, Additive y) =>
  [(Interval x, y)] ->
  [(Interval x, y)]
nestings = nestingsAsc . Heap.fromList

nestingsAsc ::
  (Ord x, Ord y, Additive y) =>
  Heap (Interval x, y) ->
  [(Interval x, y)]
nestingsAsc heap = case firstTwo of
  Nothing -> Data.toList heap
  Just ((i', iy), (j', jy), js) -> case Interval.adjacency i' j' of
    Before i j -> (i, iy) : nestingsAsc (Heap.insert (j, jy) js)
    Meets i j k ->
      (i, iy) : nestingsAsc (Heap.fromList [(j, iy + jy), (k, jy)] <> js)
    Overlaps i j k ->
      nestingsAsc do
        Heap.fromList [(i, iy), (j, iy + jy), (k, jy)] <> js
    Starts i j ->
      nestingsAsc do
        Heap.fromList [(i, iy + jy), (j, jy)] <> js
    During i j k ->
      nestingsAsc do
        Heap.fromList [(i, jy), (j, iy + jy), (k, jy)] <> js
    Finishes i j ->
      nestingsAsc do
        Heap.fromList [(i, iy), (j, iy + jy)] <> js
    Identical i -> nestingsAsc (Heap.insert (i, iy + jy) js)
    FinishedBy i j ->
      nestingsAsc do
        Heap.fromList [(i, iy), (j, iy + jy)] <> js
    Contains i j k ->
      nestingsAsc do
        Heap.fromList [(i, iy), (j, iy + jy), (k, iy)] <> js
    StartedBy i j ->
      nestingsAsc do
        Heap.fromList [(i, iy + jy), (j, iy)] <> js
    OverlappedBy i j k ->
      nestingsAsc do
        Heap.fromList [(i, jy), (j, iy + jy), (k, iy)] <> js
    MetBy i j k ->
      (i, jy) : nestingsAsc (Heap.fromList [(j, iy + jy), (k, iy)] <> js)
    After i j -> (i, jy) : nestingsAsc (Heap.insert (j, iy) js)
 where
  firstTwo = do
    (min1, heap') <- Heap.uncons heap
    (min2, heap'') <- Heap.uncons heap'
    pure (min1, min2, heap'')
