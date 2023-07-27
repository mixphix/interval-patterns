module Data.Interval.Layers (
  Layers,
  Data.Interval.Layers.fromList,
  Data.Interval.Layers.toList,
  empty,
  singleton,
  insert,
  pile,
  squash,
  thickness,
  thickest,
  dig,
  remove,
  (\-),
  baseline,
  difference,
  truncate,
  (\=),
  toStepFunction,
  integrate,

  -- ** Helper functions
  nestings,
) where

import Algebra.Lattice.Levitated (Levitated (Top))
import Data.Data (Data, Typeable)
import Data.Foldable qualified as Foldable
import Data.Group (Group (..))
import Data.Heap (Heap)
import Data.Heap qualified as Heap
import Data.Interval (
  Adjacency (..),
  Interval,
  OneOrTwo (..),
  pattern Whole,
  pattern (:---:),
  pattern (:<>:),
 )
import Data.Interval qualified as Interval
import Data.Interval.Borel (Borel)
import Data.Interval.Borel qualified as Borel
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Generics (Generic)
import Prelude hiding (truncate)

-- The 'Layers' of an ordered type @x@ are like the 'Borel' sets,
-- but that keeps track of how far each point has been "raised" in @y@.
newtype Layers x y = Layers (Map (Interval x) y)
  deriving (Eq, Ord, Show, Functor, Generic, Typeable, Data)

instance (Ord x, Ord y, Semigroup y) => Semigroup (Layers x y) where
  (<>) :: (Ord x, Ord y, Semigroup y) => Layers x y -> Layers x y -> Layers x y
  Layers s1 <> Layers s2 =
    Layers . Map.fromList . nestingsAsc . Heap.fromList $
      Map.toAscList (Map.unionWith (<>) s1 s2)

instance (Ord x, Ord y, Semigroup y) => Monoid (Layers x y) where
  mempty :: (Ord x, Ord y, Semigroup y) => Layers x y
  mempty = Layers mempty

instance (Ord x, Ord y, Group y) => Group (Layers x y) where
  invert :: (Ord x, Ord y, Group y) => Layers x y -> Layers x y
  invert (Layers s) = Layers (fmap invert s)

-- | A blank canvas.
empty :: Layers x y
empty = Layers Map.empty

-- | @singleton ix y@ is the rectangle with base @ix@ of thickness @y@.
singleton :: (Ord x) => Interval x -> y -> Layers x y
singleton ix y = Layers (Map.singleton ix y)

-- | Draw the 'Layers' of specified bases and thicknesses.
fromList :: (Ord x, Ord y, Semigroup y) => [(Interval x, y)] -> Layers x y
fromList = Layers . Map.fromList . nestings

-- | Get all of the bases and thicknesses in the 'Layers'.
toList :: (Ord x) => Layers x y -> [(Interval x, y)]
toList (Layers s) = Map.toList s

-- | Ignore the 'Layers' and focus only on whether points are 'within'
-- any contained 'Interval' or not.
squash :: (Ord x) => Layers x y -> Borel x
squash (Layers s) = foldMap Borel.singleton (Map.keys s)

-- | @insert ix y l@ draws over @l@ a rectangle with base @ix@ of thickness @y@.
insert ::
  (Ord x, Ord y, Semigroup y) =>
  Interval x ->
  y ->
  Layers x y ->
  Layers x y
insert ix y = (<>) (singleton ix y)

-- | Flipped synonym for 'insert'.
-- Mnemonic: "pile" this much onto the existing 'Layers'
-- over the given 'Interval'.
pile ::
  (Ord x, Ord y, Semigroup y) =>
  y ->
  Interval x ->
  Layers x y ->
  Layers x y
pile = flip insert

-- | Take away a thickness over a given base from the 'Layers'.
dig :: (Ord x, Ord y, Group y) => y -> Interval x -> Layers x y -> Layers x y
dig y ix = insert ix (invert y)

-- | Completely remove an 'Interval' from the 'Layers'.
remove :: (Ord x, Ord y, Semigroup y) => Interval x -> Layers x y -> Layers x y
remove ix (Layers s) = flip (`Map.foldlWithKey'` empty) s \acc jx y ->
  acc <> case jx Interval.\\ ix of
    Nothing -> mempty
    Just (One kx) -> singleton kx y
    Just (Two kx lx) -> fromList [(kx, y), (lx, y)]

-- | Fliped infix version of 'remove'.
(\-) :: (Ord x, Ord y, Semigroup y) => Layers x y -> Interval x -> Layers x y
(\-) = flip remove

-- | Add the given thickness to every point.
baseline :: (Ord x, Ord y, Semigroup y) => y -> Layers x y -> Layers x y
baseline = insert Whole

-- | "Excavate" the second argument from the first.
difference :: (Ord x, Ord y, Group y) => Layers x y -> Layers x y -> Layers x y
difference layers (Layers s) =
  foldr (uncurry (flip dig)) layers (Map.toAscList s)

-- | Restrict the range of the 'Layers' to the given 'Interval'.
truncate ::
  (Ord x, Ord y, Semigroup y) => Interval x -> Layers x y -> Layers x y
truncate ix (Layers s) =
  flip (`Map.foldlWithKey'` empty) s \acc jx y ->
    maybe id (`insert` y) (Interval.intersect ix jx) acc

-- | Flipped infix version of 'truncate'.
(\=) :: (Ord x, Ord y, Semigroup y) => Layers x y -> Interval x -> Layers x y
(\=) = flip truncate

-- |
-- @'integrate' diff hgt ix l@ calculates the area under the 'Interval' @ix@
-- using the measure @diff@ of the interval multiplied by the height @hgt@
-- of the layers over each sub-interval in the layers.
integrate ::
  (Ord x, Ord y, Semigroup y, Num z) =>
  (x -> x -> z) ->
  (y -> z) ->
  Interval x ->
  Layers x y ->
  Maybe z
integrate diff hgt ix layers =
  let Layers (Map.assocs -> s) = layers \= ix
      f (jx, y) maccum = do
        acc <- maccum
        d <- Interval.measuring diff jx
        pure $ acc + d * hgt y
   in foldr f (Just 0) s

-- | Get the thickness of the 'Layers' at a point.
thickness :: (Ord x, Semigroup y) => x -> Layers x y -> Maybe y
thickness x (Layers s) = case Map.lookupLE (x :<>: x) s of
  Just (ix, y) | x `Interval.within` ix -> Just y
  _ -> Nothing

-- | Where and how thick is the thickest 'Interval'?
thickest :: (Ord x, Ord y) => Layers x y -> Maybe (Interval x, y)
thickest (Layers s) =
  flip (`Map.foldlWithKey'` Nothing) s \acc ix y -> Just case acc of
    Nothing -> (ix, y)
    Just (_, y') | y > y' -> (ix, y)
    Just (ix', y') -> (ix', y')

-- | Convert the 'Layers' into a list of beginning-points and heights,
-- that define a step function piecewise.
toStepFunction :: (Ord x, Ord y, Monoid y) => Layers x y -> [(Levitated x, y)]
toStepFunction = go . Data.Interval.Layers.toList
 where
  go = \case
    [(il :---: iu, iy), (j@(jl :---: Top), jy)]
      | iu == jl -> (il, iy) : go [(j, jy)]
      | otherwise -> (il, iy) : (iu, mempty) : go [(j, jy)]
    (il :---: iu, iy) : (j@(jl :---: _), jy) : is
      | iu == jl -> (il, iy) : go ((j, jy) : is)
      | otherwise -> (il, iy) : (iu, mempty) : go ((j, jy) : is)
    [(il :---: iu, iy)] -> [(il, iy), (iu, mempty)]
    [] -> []

nestings ::
  (Ord x, Ord y, Semigroup y) =>
  [(Interval x, y)] ->
  [(Interval x, y)]
nestings = nestingsAsc . Heap.fromList

nestingsAsc ::
  (Ord x, Ord y, Semigroup y) =>
  Heap (Interval x, y) ->
  [(Interval x, y)]
nestingsAsc heap = case firstTwo of
  Nothing -> Foldable.toList heap
  Just ((i', iy), (j', jy), js) -> case Interval.adjacency i' j' of
    Before i j -> (i, iy) : nestingsAsc (Heap.insert (j, jy) js)
    Meets i j k ->
      (i, iy) : nestingsAsc (Heap.fromList [(j, iy <> jy), (k, jy)] <> js)
    Overlaps i j k ->
      nestingsAsc $
        Heap.fromList [(i, iy), (j, iy <> jy), (k, jy)] <> js
    Starts i j ->
      nestingsAsc $
        Heap.fromList [(i, iy <> jy), (j, jy)] <> js
    During i j k ->
      nestingsAsc $
        Heap.fromList [(i, jy), (j, iy <> jy), (k, jy)] <> js
    Finishes i j ->
      nestingsAsc $
        Heap.fromList [(i, iy), (j, iy <> jy)] <> js
    Identical i -> nestingsAsc (Heap.insert (i, iy <> jy) js)
    FinishedBy i j ->
      nestingsAsc $
        Heap.fromList [(i, iy), (j, iy <> jy)] <> js
    Contains i j k ->
      nestingsAsc $
        Heap.fromList [(i, iy), (j, iy <> jy), (k, iy)] <> js
    StartedBy i j ->
      nestingsAsc $
        Heap.fromList [(i, iy <> jy), (j, iy)] <> js
    OverlappedBy i j k ->
      nestingsAsc $
        Heap.fromList [(i, jy), (j, iy <> jy), (k, iy)] <> js
    MetBy i j k ->
      (i, jy) : nestingsAsc (Heap.fromList [(j, iy <> jy), (k, iy)] <> js)
    After i j -> (i, jy) : nestingsAsc (Heap.insert (j, iy) js)
 where
  firstTwo = do
    (min1, heap') <- Heap.uncons heap
    (min2, heap'') <- Heap.uncons heap'
    pure (min1, min2, heap'')
