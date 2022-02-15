module Data.Interval.Layers
  ( Layers,
    Data.Interval.Layers.fromList,
    Data.Interval.Layers.toList,
    empty,
    singleton,
    insert,
    squash,
    thickness,
    thickest,
    -- dig,

    -- ** Helper functiyns
    nestings,
  )
where

import Data.Interval (Adjacency (..), Interval, pattern (:<>:))
import Data.Interval qualified as I
import Data.Interval.Covering (Covering)
import Data.Interval.Covering qualified as Covering
import Data.Map.Strict qualified as Map
import Prelude hiding (empty)

-- The 'Layers' of an ordered type @x@ are like a 'Covering',
-- but that keeps track of how far each point has been "raised" in @y@.
newtype Layers x y = Layers (Map (Interval x) y)
  deriving (Eq, Ord, Show, Generic, Typeable)

instance (Ord x, Semigroup y) => Semigroup (Layers x y) where
  Layers s1 <> Layers s2 =
    Layers . Map.fromList . nestings $
      Map.toAscList s1 <> Map.toAscList s2

instance (Ord x, Semigroup y) => Monoid (Layers x y) where
  mempty = Layers mempty

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
squash :: (Ord x) => Layers x y -> Covering x
squash (Layers s) = foldMap Covering.singleton (Map.keys s)

-- | Get the thickness of the 'Layers' at a point.
thickness :: (Ord x, Monoid y) => x -> Layers x y -> y
thickness x (Layers s) = case Map.lookupLE (x :<>: x) s of
  Nothing -> mempty
  Just (ix, y) -> bool mempty y (x `I.within` ix)

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

-- | @insert ix y l@ draws over @l@ a rectangle with base @ix@ of thickness @y@.
insert ::
  (Ord x, Semigroup y) =>
  Interval x ->
  y ->
  Layers x y ->
  Layers x y
insert ix y = mappend (singleton ix y)

nestings ::
  (Ord x, Semigroup y) =>
  [(Interval x, y)] ->
  [(Interval x, y)]
nestings = \case
  (a, ay) : (b, by) : js ->
    let ((i', iy), (j', jy))
          | a <= b = ((a, ay), (b, by))
          | otherwise = ((b, by), (a, ay))
     in case I.adjacency i' j' of
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

-- dig :: (Ord x, Group y) => Interval x -> y -> Layers x y -> Layers x y
-- dig ix y (Layers s) = _
