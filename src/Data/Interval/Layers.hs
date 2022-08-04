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

import Algebra.Lattice.Levitated
import Data.Data
import Data.Group (Group (..))
import Data.Interval (Adjacency (..), Interval, OneOrTwo (..), pattern Whole, pattern (:---:), pattern (:<>:))
import Data.Interval qualified as I
import Data.Interval.Borel (Borel)
import Data.Interval.Borel qualified as Borel
import Data.Map.Strict qualified as Map
import Prelude hiding (empty, fromList, truncate)

-- The 'Layers' of an ordered type @x@ are like the 'Borel' sets,
-- but that keeps track of how far each point has been "raised" in @y@.
newtype Layers x y = Layers (Map (Interval x) y)
  deriving (Eq, Ord, Show, Functor, Generic, Typeable, Data)

instance (Ord x, Semigroup y) => Semigroup (Layers x y) where
  Layers s1 <> Layers s2 =
    let s = Map.toAscList $ Map.unionWith (<>) s1 s2
     in Layers $ Map.fromList (nestingsAsc s)

instance (Ord x, Semigroup y) => Monoid (Layers x y) where
  mempty = Layers mempty

instance (Ord x, Group y) => Group (Layers x y) where
  invert (Layers s) = Layers (fmap invert s)

-- | A blank canvas.
empty :: Layers x y
empty = Layers Map.empty

-- | @singleton ix y@ is the rectangle with base @ix@ of thickness @y@.
singleton :: (Ord x) => Interval x -> y -> Layers x y
singleton ix y = Layers (Map.singleton ix y)

-- | Draw the 'Layers' of specified bases and thicknesses.
fromList :: (Ord x, Semigroup y) => [(Interval x, y)] -> Layers x y
fromList = foldMap (uncurry singleton)

-- | Get all of the bases and thicknesses in the 'Layers'.
toList :: (Ord x) => Layers x y -> [(Interval x, y)]
toList (Layers s) = Map.toList s

-- | Ignore the 'Layers' and focus only on whether points are 'within'
-- any contained 'Interval' or not.
squash :: (Ord x) => Layers x y -> Borel x
squash (Layers s) = foldMap Borel.singleton (Map.keys s)

-- | @insert ix y l@ draws over @l@ a rectangle with base @ix@ of thickness @y@.
insert ::
  (Ord x, Semigroup y) =>
  Interval x ->
  y ->
  Layers x y ->
  Layers x y
insert ix y = (<>) (singleton ix y)

-- | Flipped synonym for 'insert'.
-- Mnemonic: "pile" this much onto the existing 'Layers'
-- over the given 'Interval'.
pile ::
  (Ord x, Semigroup y) =>
  y ->
  Interval x ->
  Layers x y ->
  Layers x y
pile = flip insert

-- | Take away a thickness over a given base from the 'Layers'.
dig :: (Ord x, Group y) => y -> Interval x -> Layers x y -> Layers x y
dig y ix = insert ix (invert y)

-- | Completely remove an 'Interval' from the 'Layers'.
remove :: (Ord x, Semigroup y) => Interval x -> Layers x y -> Layers x y
remove ix (Layers s) =
  Map.foldlWithKey'
    ( \acc jx y -> case jx I.\\ ix of
        Nothing -> acc
        Just (One kx) -> acc <> singleton kx y
        Just (Two kx lx) -> acc <> fromList [(kx, y), (lx, y)]
    )
    empty
    s

-- | Fliped infix version of 'remove'.
(\-) :: (Ord x, Semigroup y) => Layers x y -> Interval x -> Layers x y
(\-) = flip remove

-- | Add the given thickness to every point.
baseline :: (Ord x, Semigroup y) => y -> Layers x y -> Layers x y
baseline = insert Whole

-- | "Excavate" the second argument from the first.
difference :: (Ord x, Group y) => Layers x y -> Layers x y -> Layers x y
difference layers (Layers s) =
  foldr (uncurry (flip dig)) layers (Map.toAscList s)

-- | Restrict the range of the 'Layers' to the given 'Interval'.
truncate :: (Ord x, Semigroup y) => Interval x -> Layers x y -> Layers x y
truncate ix (Layers s) =
  Map.foldlWithKey'
    ( \acc jx y -> case I.intersect ix jx of
        Nothing -> acc
        Just x -> insert x y acc
    )
    empty
    s

-- | Flipped infix version of 'truncate'.
(\=) :: (Ord x, Semigroup y) => Layers x y -> Interval x -> Layers x y
(\=) = flip truncate

-- |
-- @'integrate' diff hgt ix l@ calculates the area under the 'Interval' @ix@
-- using the measure @diff@ of the interval multiplied by the height @hgt@
-- of the layers over each sub-interval in the layers.
integrate ::
  (Ord x, Semigroup y, Num z) =>
  (x -> x -> z) ->
  (y -> z) ->
  Interval x ->
  Layers x y ->
  Maybe z
integrate diff hgt ix layers =
  let Layers (Map.assocs -> s) = layers \= ix
      f (jx, y) maccum = do
        acc <- maccum
        d <- I.measuring diff jx
        pure $ acc + d * hgt y
   in foldr f (Just 0) s

-- | Get the thickness of the 'Layers' at a point.
thickness :: (Ord x, Monoid y) => x -> Layers x y -> y
thickness x (Layers s) = case Map.lookupLE (x :<>: x) s of
  Just (ix, y) | x `I.within` ix -> y
  _ -> mempty

-- | Where and how thick is the thickest 'Interval'?
thickest :: (Ord x, Ord y) => Layers x y -> Maybe (Interval x, y)
thickest (Layers s) =
  Map.foldlWithKey'
    ( \acc ix y -> Just $ case acc of
        Nothing -> (ix, y)
        Just (ix', y') -> if y > y' then (ix, y) else (ix', y')
    )
    Nothing
    s

-- | Convert the 'Layers' into a list of beginning-points and heights,
-- that define a step function piecewise.
toStepFunction :: (Ord x, Monoid y) => Layers x y -> [(Levitated x, y)]
toStepFunction s = g (Data.Interval.Layers.toList $ baseline mempty s)
 where
  g [(il :---: iu, iy), (j@(jl :---: Top), jy)]
    | iu == jl = (il, iy) : g [(j, jy)]
    | otherwise = (il, iy) : (iu, mempty) : g [(j, jy)]
  g ((il :---: iu, iy) : (j@(jl :---: _), jy) : is)
    | iu == jl = (il, iy) : g ((j, jy) : is)
    | otherwise = (il, iy) : (iu, mempty) : g ((j, jy) : is)
  g [] = []
  g [(il :---: iu, iy)] = [(il, iy), (iu, mempty)]

nestings ::
  (Ord x, Semigroup y) =>
  [(Interval x, y)] ->
  [(Interval x, y)]
nestings = nestingsAsc . sortOn fst

nestingsAsc ::
  (Ord x, Semigroup y) =>
  [(Interval x, y)] ->
  [(Interval x, y)]
nestingsAsc = \case
  (i', iy) : (j', jy) : js -> case I.adjacency i' j' of
    Before i j -> (i, iy) : nestings ((j, jy) : js)
    Meets i j k -> (i, iy) : nestings ((j, iy <> jy) : (k, jy) : js)
    Overlaps i j k ->
      nestings $
        (i, iy) :
        (j, iy <> jy) :
        (k, jy) : js
    Starts i j ->
      nestings $
        (i, iy <> jy) :
        (j, jy) : js
    During i j k ->
      nestings $
        (i, iy) :
        (j, iy <> jy) :
        (k, jy) : js
    Finishes i j ->
      nestings $
        (i, iy) :
        (j, iy <> jy) : js
    Identical i -> (i, iy <> jy) : nestings js
    FinishedBy i j ->
      nestings $
        (i, iy) :
        (j, iy <> jy) : js
    Contains i j k ->
      nestings $
        (i, iy) :
        (j, iy <> jy) :
        (k, jy) : js
    StartedBy i j ->
      nestings $
        (i, iy <> jy) :
        (j, jy) : js
    OverlappedBy i j k ->
      nestings $
        (i, iy) :
        (j, iy <> jy) :
        (k, jy) : js
    MetBy i j k -> (i, iy) : nestings ((j, iy <> jy) : (k, jy) : js)
    After i j -> (i, iy) : nestings ((j, jy) : js)
  x -> x
