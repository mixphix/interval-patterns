{-# LANGUAGE MagicHash #-}

-- |
-- Module       : Data.Interval
-- Copyright    : (c) Melanie Brown 2022
-- License:     : BSD3 (see the file LICENSE)
--
-- Intervals over types and their operations.
module Data.Interval
  ( module Data.Interval.Adjacency,
    module Data.Interval.Types,
    withBounds,
    imin,
    iinf,
    isup,
    imax,
    hull,
    hulls,
    within,
    point,
    open,
    close,
    openclosed,
    closedopen,
    openLower,
    closedLower,
    openUpper,
    closedUpper,
    setLower,
    setUpper,
    adjacency,
    split,
    intersect,
    union,
    unions,
    complement,
    difference,
    symmetricDifference,
    measure,
    measuring,
    hausdorff,
    (+/-),
  )
where

import Control.Exception.Base (typeError)
import Data.Interval.Adjacency
import Data.Interval.Types
import Data.OneOrTwo (OneOrTwo (..))
import Data.Suspension (Suspension (..))

withBounds ::
  (Ord x) =>
  SomeBound (Suspension x) ->
  SomeBound (Suspension x) ->
  Interval x
withBounds (SomeBound b1) (SomeBound b2) = case (b1, b2) of
  (Min l, Max u) -> l :|-|: u
  (Inf l, Max u) -> l :<-|: u
  (Min l, Sup u) -> l :|->: u
  (Inf l, Sup u) -> l :<->: u
  (Max u, Min l) -> l :|-|: u
  (Max u, Inf l) -> l :<-|: u
  (Sup u, Min l) -> l :|->: u
  (Sup u, Inf l) -> l :<->: u
  _ -> typeError "cannot make an interval with the given bounds"#

-- | Get the convex hull of two intervals.
--
-- >>> hull (3 :||: 4) (7 :|>: 8)
-- (Merid 3 :|->: Merid 8)
--
-- >>> hull (Nadir :<|: 3) (3 :<|: 4)
-- (Merid Nadir :<-|: Merid (Merid 4))
hull :: (Ord x) => Interval x -> Interval x -> Interval x
hull i1 i2 =
  let [il, iu] = sort [i1, i2]
   in case (lowerBound il, upperBound iu) of
        (SomeBound l@(Inf _), SomeBound u@(Sup _)) -> l :<-->: u
        (SomeBound l@(Inf _), SomeBound u@(Max _)) -> l :<--|: u
        (SomeBound l@(Min _), SomeBound u@(Sup _)) -> l :|-->: u
        (SomeBound l@(Min _), SomeBound u@(Max _)) -> l :|--|: u
        _ -> typeError "Invalid lower/upper bounds"#

-- | Get the convex hull of a non-empty list of intervals.
hulls :: (Ord x) => NonEmpty (Interval x) -> Interval x
hulls (i :| []) = i
hulls (i :| j : is) = hulls $ hull i j :| is

-- | Test whether a point is contained in the interval.
within :: (Ord x) => x -> Interval x -> Bool
within (Merid -> x) = \case
  l :<->: u -> l < x && x < u
  l :<-|: u -> l < x && x <= u
  l :|->: u -> l <= x && x < u
  l :|-|: u -> l <= x && x <= u

-- | Create the closed-closed interval at a given point.
point :: (Ord x) => x -> Interval x
point = join (:||:)

-- | Get the minimum of an interval, if it exists.
imin :: (Ord x) => Interval x -> Maybe (Bound Minimum (Suspension x))
imin = \case
  (_ :<-->: _) -> Nothing
  (_ :<--|: _) -> Nothing
  (x :|-->: _) -> Just x
  (x :|--|: _) -> Just x

-- | Get the infimum of an interval, weakening if necessary.
iinf :: (Ord x) => Interval x -> Bound Infimum (Suspension x)
iinf = \case
  (x :<->: _) -> Inf x
  (x :<-|: _) -> Inf x
  (x :|->: _) -> Inf x
  (x :|-|: _) -> Inf x

-- | Get the supremum of an interval, weakening if necessary.
isup :: (Ord x) => Interval x -> Bound Supremum (Suspension x)
isup = \case
  (_ :<->: x) -> Sup x
  (_ :<-|: x) -> Sup x
  (_ :|->: x) -> Sup x
  (_ :|-|: x) -> Sup x

-- | Get the maximum of an interval if it exists.
imax :: (Ord x) => Interval x -> Maybe (Bound Maximum (Suspension x))
imax = \case
  (_ :<-->: _) -> Nothing
  (_ :<--|: x) -> Just x
  (_ :|-->: _) -> Nothing
  (_ :|--|: x) -> Just x

-- | Open both bounds of the given interval.
open :: (Ord x) => Interval x -> Interval x
open = \case
  l :<->: u -> l :<->: u
  l :<-|: u -> l :<->: u
  l :|->: u -> l :<->: u
  l :|-|: u -> l :<->: u

-- | Close both bounds of the given interval.
close :: (Ord x) => Interval x -> Interval x
close = \case
  l :<->: u -> l :|-|: u
  l :<-|: u -> l :|-|: u
  l :|->: u -> l :|-|: u
  l :|-|: u -> l :|-|: u

-- | Make the interval open-closed, leaving the endpoints unchanged.
openclosed :: (Ord x) => Interval x -> Interval x
openclosed = \case
  l :<->: u -> l :<->: u
  l :<-|: u -> l :<->: u
  l :|->: u -> l :<->: u
  l :|-|: u -> l :<->: u

-- | Make the interval closed-open, leaving the endpoints unchanged.
closedopen :: (Ord x) => Interval x -> Interval x
closedopen = \case
  l :<->: u -> l :|-|: u
  l :<-|: u -> l :|-|: u
  l :|->: u -> l :|-|: u
  l :|-|: u -> l :|-|: u

-- | Make the lower bound open, leaving the endpoints unchanged.
openLower :: (Ord x) => Interval x -> Interval x
openLower = \case
  l :<->: u -> l :<->: u
  l :<-|: u -> l :<-|: u
  l :|->: u -> l :<->: u
  l :|-|: u -> l :<-|: u

-- | Make the lower bound closed, leaving the endpoints unchanged.
closedLower :: (Ord x) => Interval x -> Interval x
closedLower = \case
  l :<->: u -> l :|->: u
  l :<-|: u -> l :|-|: u
  l :|->: u -> l :|->: u
  l :|-|: u -> l :|-|: u

-- | Make the upper bound open, leaving the endpoints unchanged.
openUpper :: (Ord x) => Interval x -> Interval x
openUpper = \case
  l :<->: u -> l :<->: u
  l :<-|: u -> l :<->: u
  l :|->: u -> l :|->: u
  l :|-|: u -> l :|->: u

-- | Make the upper bound closed, leaving the endpoints unchanged.
closedUpper :: (Ord x) => Interval x -> Interval x
closedUpper = \case
  l :<->: u -> l :<-|: u
  l :<-|: u -> l :<-|: u
  l :|->: u -> l :|-|: u
  l :|-|: u -> l :|-|: u

setLower :: (Ord x) => Suspension x -> Interval x -> Interval x
setLower x = \case
  _ :<->: u -> x :<->: u
  _ :<-|: u -> x :<-|: u
  _ :|->: u -> x :|->: u
  _ :|-|: u -> x :|-|: u

setUpper :: (Ord x) => Suspension x -> Interval x -> Interval x
setUpper x = \case
  l :<->: _ -> l :<->: x
  l :<-|: _ -> l :<-|: x
  l :|->: _ -> l :|->: x
  l :|-|: _ -> l :|-|: x

-- | Calculate the adjacency relationship between two intervals, according to
-- [Allen](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra).
adjacency :: (Ord x) => Interval x -> Interval x -> Adjacency
adjacency i1 i2 =
  case (on compare lower i1 i2, on compare upper i1 i2) of
    (LT, LT) -> case u1 `compare` l2 of
      LT -> Before
      EQ -> case (ub1, lb2) of
        (Maximum, Minimum) -> Meets
        _ -> Before
      GT -> Overlaps
    (LT, EQ) -> Finishes
    (LT, GT) -> Contains
    (EQ, LT) -> Starts
    (EQ, EQ) -> Identical
    (EQ, GT) -> StartedBy
    (GT, LT) -> During
    (GT, EQ) -> FinishedBy
    (GT, GT) -> case u2 `compare` l1 of
      GT -> OverlappedBy
      EQ -> case (ub2, lb1) of
        (Maximum, Minimum) -> MetBy
        _ -> After
      LT -> After
  where
    (l1, lb1) = lower i1
    (l2, lb2) = lower i2
    (u1, ub1) = upper i1
    (u2, ub2) = upper i2

-- | Two intervals can overlap and be 'split' into at most 3 distinct intervals:
--
--  - if they are identical, then the left argument is returned;
--  - if they do not overlap, they are returned in lexicographical order;
--  - if they do overlap:
--    - and they do not share an endpoint, the intersection is returned
--      as the middle of three, with the others having that portion removed;
--    - and they share an endpoint, the intersection is returned
--      in the side of the two where the endpoint matches.
split :: (Ord x) => Interval x -> Interval x -> SomeAdjacency x
split i1 i2 = case adjacency i1 i2 of
  Before -> SomeAdjacency $ BeforeJ i1 i2
  Meets ->
    SomeAdjacency $
      MeetsJ
        (openUpper i1)
        (withBounds lb2 ub1)
        (openLower i2)
  Overlaps ->
    SomeAdjacency $
      OverlapsJ
        (withBounds lb1 (oppose lb2))
        (withBounds lb2 ub1)
        (withBounds (oppose ub1) ub2)
  Starts ->
    SomeAdjacency $
      StartsJ
        i1
        (withBounds (oppose ub1) ub2)
  During ->
    SomeAdjacency $
      DuringJ
        (withBounds lb2 (oppose lb1))
        (withBounds lb1 ub1)
        (withBounds (oppose ub1) ub2)
  Finishes ->
    SomeAdjacency $
      FinishesJ
        (withBounds lb1 (oppose lb2))
        i2
  Identical -> SomeAdjacency (IdenticalJ i1)
  FinishedBy ->
    SomeAdjacency $
      FinishedByJ
        (withBounds lb2 (oppose lb1))
        i1
  Contains ->
    SomeAdjacency $
      ContainsJ
        (withBounds lb1 (oppose lb2))
        (withBounds lb2 ub2)
        (withBounds (oppose ub2) ub1)
  StartedBy ->
    SomeAdjacency $
      StartedByJ
        i2
        (withBounds (oppose ub2) ub1)
  OverlappedBy ->
    SomeAdjacency $
      OverlappedByJ
        (withBounds lb2 (oppose lb1))
        (withBounds lb1 ub2)
        (withBounds (oppose ub2) ub1)
  MetBy ->
    SomeAdjacency $
      MetByJ
        (openUpper i2)
        (withBounds lb1 ub2)
        (openLower i1)
  After -> SomeAdjacency $ AfterJ i2 i1
  where
    (lb1, ub1) = bounds i1
    (lb2, ub2) = bounds i2

-- | Calculate the intersection of two intervals, if it exists.
--
-- @
--
-- >>> intersect (2 :<>: 4) (3 :||: 5)
-- Just (Merid 3 :|->: Merid 4)
--
-- >>> intersect (2 :<>: 4) (4 :||: 5)
-- Nothing
--
-- >>> intersect (1 :<>: 4) (2 :||: 3)
-- Just (Merid 2 :|-|: Merid 3)
--
-- @
intersect ::
  forall x.
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (Interval x)
intersect i1 i2 = case split i1 i2 of
  SomeAdjacency (adj :: Adjacent adj x) -> case adj of
    BeforeJ _ _ -> Nothing
    MeetsJ _ j _ -> Just j
    OverlapsJ _ j _ -> Just j
    StartsJ i _ -> Just i
    DuringJ _ j _ -> Just j
    FinishesJ _ j -> Just j
    IdenticalJ i -> Just i
    FinishedByJ _ j -> Just j
    ContainsJ _ j _ -> Just j
    StartedByJ i _ -> Just i
    OverlappedByJ _ j _ -> Just j
    MetByJ _ j _ -> Just j
    AfterJ _ _ -> Nothing

-- | Get the union of two intervals, as either 'OneOrTwo'.
--
-- @
--
-- >>> union (2 :||: 5) (5 :<>: 7)
-- One (Merid 2 :|->: Merid 7)
--
-- >>> union (2 :||: 4) (5 :<>: 7)
-- Two (Merid 2 :|-|: Merid 4) (Merid 5 :<->: Merid 7)
--
-- @
union ::
  forall x.
  (Ord x) =>
  Interval x ->
  Interval x ->
  OneOrTwo (Interval x)
union i1 i2 = case split i1 i2 of
  SomeAdjacency (adj :: Adjacent adj x) -> case adj of
    BeforeJ i j
      | fst (upper i) == fst (lower j) -> One $ hull i j
      | otherwise -> Two i j
    MeetsJ i j k -> One $ hulls (k :| [hull i j])
    OverlapsJ i j k -> One $ hulls (i :| [j, k])
    StartsJ i j -> One $ hulls (i :| [j])
    DuringJ i j k -> One $ hulls (i :| [j, k])
    FinishesJ i j -> One $ hulls (i :| [j])
    IdenticalJ i -> One i
    FinishedByJ i j -> One $ hulls (i :| [j])
    ContainsJ i j k -> One $ hulls (i :| [j, k])
    StartedByJ i j -> One $ hulls (i :| [j])
    OverlappedByJ i j k -> One $ hulls (i :| [j, k])
    MetByJ i j k -> One $ hulls (k :| [hull i j])
    AfterJ i j
      | fst (upper i) == fst (lower j) -> One $ hull i j
      | otherwise -> Two i j

-- | Get the union of a list of intervals.
unions :: forall x. (Ord x) => [Interval x] -> [Interval x]
unions = foldr f []
  where
    f i [] = [i]
    f i (j : js) = case i `union` j of
      One i' -> f i' js
      _ -> j : f i js

-- | Take the complement of the interval, as possibly 'OneOrTwo'.
--
-- @
--
-- >>> complement (3 :<>: 4)
-- Just (Two (Nadir :|-|: Merid 3) (Merid 4 :|-|: Zenit))
--
-- @
complement :: forall x. (Ord x) => Interval x -> Maybe (OneOrTwo (Interval x))
complement = \case
  Whole -> Nothing
  Nadir :|-|: u -> Just (One (u :<-|: Zenit))
  Nadir :|->: u -> Just (One (u :|-|: Zenit))
  Nadir :<-|: u -> Just (Two (Nadir :|-|: Nadir) (u :<-|: Zenit))
  Nadir :<->: u -> Just (Two (Nadir :|-|: Nadir) (u :|-|: Zenit))
  --
  l :|-|: Zenit -> Just (One (Nadir :|->: l))
  l :<-|: Zenit -> Just (One (Nadir :|-|: l))
  l :|->: Zenit -> Just (Two (Nadir :|->: l) (Zenit :|-|: Zenit))
  l :<->: Zenit -> Just (Two (Nadir :|-|: l) (Zenit :|-|: Zenit))
  --
  l :|-|: u -> Just (Two (Nadir :|->: l) (u :<-|: Zenit))
  l :|->: u -> Just (Two (Nadir :|->: l) (u :|-|: Zenit))
  l :<-|: u -> Just (Two (Nadir :|-|: l) (u :<-|: Zenit))
  l :<->: u -> Just (Two (Nadir :|-|: l) (u :|-|: Zenit))

-- | Remove all points of the second interval from the first.
--
-- @
--
-- >>> difference Whole (3 :<>: 4)
-- Just (Two (Nadir :|-|: Merid 3) (Merid 4 :|-|: Zenit))
--
-- >>> difference (1 :<>: 4) (2 :||: 3)
-- Just (Two (Merid 1 :<->: Merid 2) (Merid 3 :<->: Merid 4))
--
-- @
difference ::
  forall x.
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
difference i1 i2 = case split i1 i2 of
  SomeAdjacency (adj :: Adjacent adj x) -> case adj of
    -- not commutative!!
    BeforeJ i _ -> Just $ One i
    MeetsJ i _ _ -> Just $ One i
    OverlapsJ i _ _ -> Just $ One i
    StartsJ {} -> Nothing
    DuringJ {} -> Nothing
    FinishesJ {} -> Nothing
    IdenticalJ {} -> Nothing
    FinishedByJ i _ -> Just $ One i
    ContainsJ i _ k -> Just $ Two i k
    StartedByJ _ j -> Just $ One j
    OverlappedByJ _ _ k -> Just $ One k
    MetByJ i _ _ -> Just $ One i
    AfterJ i _ -> Just $ One i

-- | The difference of the union and intersection of two intervals.
--
-- @
--
-- >>> symmetricDifference Whole (3 :<>: 4)
-- Just (Two (Nadir :|-|: Merid 3) (Merid 4 :|-|: Zenit))
--
-- >>> symmetricDifference (1 :<>: 4) (2 :||: 3)
-- Just (Two (Merid 1 :<->: Merid 2) (Merid 3 :<->: Merid 4))
--
-- @
symmetricDifference ::
  forall x.
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
symmetricDifference i1 i2 = case i1 `union` i2 of
  Two j1 j2 -> Just (Two j1 j2)
  One u -> case i1 `intersect` i2 of
    Nothing -> Just (One u)
    Just i -> difference u i

-- | Get the measure of an interval.
--
-- @
--
-- >>> measure (-1 :<>: 1)
-- Just 2
--
-- >>> measure (Nadir :<->: 1)
-- Nothing
--
-- @
measure :: forall x. (Ord x, Num x) => Interval x -> Maybe x
measure = measuring subtract

-- | Apply a function to the lower, then upper, endpoint of an interval.
--
-- @
--
-- >>> measuring max (-1 :<>: 1)
-- Just 1
--
-- >>> measuring min (-1 :<>: 1)
-- Just (-1)
--
-- @
measuring ::
  forall y x. (Ord x, Num y) => (x -> x -> y) -> Interval x -> Maybe y
measuring f = \case
  Merid l :|-|: Merid u -> Just (f l u)
  Merid l :|->: Merid u -> Just (f l u)
  Merid l :<-|: Merid u -> Just (f l u)
  Merid l :<->: Merid u -> Just (f l u)
  l :|-|: u -> if l == u then Just 0 else Nothing
  l :|->: u -> if l == u then Just 0 else Nothing
  l :<-|: u -> if l == u then Just 0 else Nothing
  l :<->: u -> if l == u then Just 0 else Nothing

-- | Get the distance between two intervals, or 0 if they adjacency.
--
-- @
--
-- >>> hausdorff (3 :<>: 5) (6 :<>: 7)
-- Just 1
--
-- >>> hausdorff (3 :<>: 5) Whole
-- Just 0
--
-- @
hausdorff :: (Ord x, Num x) => Interval x -> Interval x -> Maybe x
hausdorff i1 i2 = case adjacency i1 i2 of
  Before -> case (upper i1, lower i2) of
    ((Merid u1, _), (Merid l2, _)) -> Just (l2 - u1)
    _ -> Nothing
  After -> case (lower i1, upper i2) of
    ((Merid l1, _), (Merid u2, _)) -> Just (l1 - u2)
    _ -> Nothing
  _ -> Just 0

-- | @m '+/-' r@ creates the closed interval centred at @m@ with radius @r@.
--
-- For the open interval, simply write @'open' (x '+/-' y)@.
(+/-) :: (Ord x, Num x) => x -> x -> Interval x
m +/- r = m - r :||: m + r
