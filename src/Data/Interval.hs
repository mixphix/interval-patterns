-- |
-- Module       : Data.Interval
-- Copyright    : (c) Melanie Brown 2022
-- License:     : BSD3 (see the file LICENSE)
--
-- Intervals over types and their operations.
module Data.Interval
  ( Extremum (..),
    opposite,
    Bound (..),
    compareBounds,
    Interval (..),
    pattern (:<->:),
    pattern (:<-|:),
    pattern (:|->:),
    pattern (:|-|:),
    pattern (:<>:),
    pattern (:<|:),
    pattern (:|>:),
    pattern (:||:),
    pattern Whole,
    orient,
    imin,
    iinf,
    isup,
    imax,
    lower,
    upper,
    hull,
    within,
    point,
    open,
    close,
    overlap,
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

import Data.Data (Data)
import Data.Interval.Overlap (Overlap (..))
import Data.OneOrTwo
import Data.Suspension (Suspension (..))
import GHC.Show qualified (show)

data Extremum
  = Minimum
  | Infimum
  | Supremum
  | Maximum
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Data, Typeable)

opposite :: Extremum -> Extremum
opposite = \case
  Minimum -> Supremum
  Infimum -> Maximum
  Supremum -> Minimum
  Maximum -> Infimum

type Bound :: Extremum -> Type -> Type
data Bound ext x where
  Min :: x -> Bound Minimum x
  Inf :: x -> Bound Infimum x
  Sup :: x -> Bound Supremum x
  Max :: x -> Bound Maximum x

instance Functor (Bound ext) where
  fmap f = \case
    Min x -> Min (f x)
    Inf x -> Inf (f x)
    Sup x -> Sup (f x)
    Max x -> Max (f x)

instance (Eq x) => Eq (Bound ext x) where
  Min x == Min y = x == y
  Inf x == Inf y = x == y
  Sup x == Sup y = x == y
  Max x == Max y = x == y

compareBounds ::
  (Ord x) =>
  Bound ext1 (Suspension x) ->
  Bound ext2 (Suspension x) ->
  Ordering
compareBounds (Min l) = \case
  Min ll -> compare l ll
  Inf ll -> compare l ll <> LT
  Sup u -> compare l u <> GT
  Max u -> compare l u
compareBounds (Inf l) = \case
  Min ll -> compare l ll <> GT
  Inf ll -> compare l ll
  Sup u -> compare l u <> GT
  Max u -> compare l u <> GT
compareBounds (Sup u) = \case
  Min l -> compare l u <> LT
  Inf l -> compare l u <> LT
  Sup uu -> compare u uu
  Max uu -> compare u uu <> LT
compareBounds (Max u) = \case
  Min l -> compare l u
  Inf l -> compare l u <> LT
  Sup uu -> compare u uu <> GT
  Max uu -> compare u uu

instance (Ord x) => Ord (Bound ext (Suspension x)) where
  compare = compareBounds

infix 5 :<-->:

infix 5 :<--|:

infix 5 :|-->:

infix 5 :|--|:

type Interval :: Type -> Type
data Interval x where
  (:<-->:) ::
    Bound Infimum (Suspension x) ->
    Bound Supremum (Suspension x) ->
    Interval x
  (:<--|:) ::
    Bound Infimum (Suspension x) ->
    Bound Maximum (Suspension x) ->
    Interval x
  (:|-->:) ::
    Bound Minimum (Suspension x) ->
    Bound Supremum (Suspension x) ->
    Interval x
  (:|--|:) ::
    Bound Minimum (Suspension x) ->
    Bound Maximum (Suspension x) ->
    Interval x

infix 5 :<->:

infix 5 :<-|:

infix 5 :|->:

infix 5 :|-|:

-- | A pattern synonym matching open intervals.
pattern (:<->:) :: Suspension a -> Suspension a -> Interval a
pattern l :<->: u = Inf l :<-->: Sup u

-- | A pattern synonym matching open-closed intervals.
pattern (:<-|:) :: Suspension a -> Suspension a -> Interval a
pattern l :<-|: u = Inf l :<--|: Max u

-- | A pattern synonym matching closed-open intervals.
pattern (:|->:) :: Suspension a -> Suspension a -> Interval a
pattern l :|->: u = Min l :|-->: Sup u

-- | A pattern synonym matching closed intervals.
pattern (:|-|:) :: Suspension a -> Suspension a -> Interval a
pattern l :|-|: u = Min l :|--|: Max u

{-# COMPLETE (:<->:), (:<-|:), (:|->:), (:|-|:) #-}

infix 5 :<>:

infix 5 :<|:

infix 5 :|>:

infix 5 :||:

-- | A pattern synonym matching finite open intervals.
pattern (:<>:) :: a -> a -> Interval a
pattern l :<>: u = Merid l :<->: Merid u

-- | A pattern synonym matching finite open-closed intervals.
pattern (:<|:) :: a -> a -> Interval a
pattern l :<|: u = Merid l :<-|: Merid u

-- | A pattern synonym matching finite closed-open intervals.
pattern (:|>:) :: a -> a -> Interval a
pattern l :|>: u = Merid l :|->: Merid u

-- | A pattern synonym matching finite closed intervals.
pattern (:||:) :: a -> a -> Interval a
pattern l :||: u = Merid l :|-|: Merid u

instance Functor Interval where
  fmap f = \case
    l1 :<->: u1 -> fmap f l1 :<->: fmap f u1
    l1 :<-|: u1 -> fmap f l1 :<-|: fmap f u1
    l1 :|->: u1 -> fmap f l1 :|->: fmap f u1
    l1 :|-|: u1 -> fmap f l1 :|-|: fmap f u1

instance (Show x) => Show (Interval x) where
  show = \case
    l :<->: u -> "(" <> show l <> " :<->: " <> show u <> ")"
    l :|->: u -> "(" <> show l <> " :|->: " <> show u <> ")"
    l :<-|: u -> "(" <> show l <> " :<-|: " <> show u <> ")"
    l :|-|: u -> "(" <> show l <> " :|-|: " <> show u <> ")"

instance (Eq x) => Eq (Interval x) where
  l1 :<->: u1 == l2 :<->: u2 = l1 == l2 && u1 == u2
  l1 :<-|: u1 == l2 :<-|: u2 = l1 == l2 && u1 == u2
  l1 :|->: u1 == l2 :|->: u2 = l1 == l2 && u1 == u2
  l1 :|-|: u1 == l2 :|-|: u2 = l1 == l2 && u1 == u2
  _ == _ = False

orient :: (Ord x) => Interval x -> Interval x
orient = \case
  l :<->: u
    | l == u -> l :|-|: u
    | otherwise -> min l u :<->: max l u
  l :<-|: u -> min l u :<-|: max l u
  l :|->: u -> min l u :|->: max l u
  l :|-|: u -> min l u :|-|: max l u

lower :: (Ord x) => Interval x -> (Suspension x, Extremum)
lower =
  orient >>> \case
    l :<->: _ -> (l, Infimum)
    l :<-|: _ -> (l, Infimum)
    l :|->: _ -> (l, Minimum)
    l :|-|: _ -> (l, Minimum)

upper :: (Ord x) => Interval x -> (Suspension x, Extremum)
upper =
  orient >>> \case
    _ :<->: u -> (u, Supremum)
    _ :<-|: u -> (u, Maximum)
    _ :|->: u -> (u, Supremum)
    _ :|-|: u -> (u, Maximum)

hull :: (Ord x) => Interval x -> Interval x -> Interval x
hull (orient -> i1) (orient -> i2) =
  case (on min lower i1 i2, on max upper i1 i2) of
    ((l, Infimum), (u, Supremum)) -> l :<->: u
    ((l, Infimum), (u, _)) -> l :<-|: u
    ((l, _), (u, Supremum)) -> l :|->: u
    ((l, _), (u, _)) -> l :|-|: u

within :: (Ord x) => x -> Interval x -> Bool
within (Merid -> x) =
  orient >>> \case
    l :<->: u -> l < x && x < u
    l :<-|: u -> l < x && x <= u
    l :|->: u -> l <= x && x < u
    l :|-|: u -> l <= x && x <= u

point :: x -> Interval x
point = join (:||:)

pattern Whole :: Interval x
pattern Whole = Nadir :|-|: Zenit

instance (Ord x) => Ord (Interval x) where
  compare i1 i2 = on compare lower i1 i2 <> on compare upper i1 i2

imin :: (Ord x) => Interval x -> Maybe (Bound Minimum (Suspension x))
imin =
  orient >>> \case
    (_ :<-->: _) -> Nothing
    (_ :<--|: _) -> Nothing
    (x :|-->: _) -> Just x
    (x :|--|: _) -> Just x

iinf :: (Ord x) => Interval x -> Bound Infimum (Suspension x)
iinf =
  orient >>> \case
    (x :<->: _) -> Inf x
    (x :<-|: _) -> Inf x
    (x :|->: _) -> Inf x
    (x :|-|: _) -> Inf x

isup :: (Ord x) => Interval x -> Bound Supremum (Suspension x)
isup =
  orient >>> \case
    (_ :<->: x) -> Sup x
    (_ :<-|: x) -> Sup x
    (_ :|->: x) -> Sup x
    (_ :|-|: x) -> Sup x

imax :: (Ord x) => Interval x -> Maybe (Bound Maximum (Suspension x))
imax =
  orient >>> \case
    (_ :<-->: _) -> Nothing
    (_ :<--|: x) -> Just x
    (_ :|-->: _) -> Nothing
    (_ :|--|: x) -> Just x

open :: (Ord x) => Interval x -> Interval x
open =
  orient >>> \case
    l :<->: u -> l :<->: u
    l :<-|: u -> l :<->: u
    l :|->: u -> l :<->: u
    l :|-|: u -> l :<->: u

close :: (Ord x) => Interval x -> Interval x
close =
  orient >>> \case
    l :<->: u -> l :|-|: u
    l :<-|: u -> l :|-|: u
    l :|->: u -> l :|-|: u
    l :|-|: u -> l :|-|: u

overlap :: (Ord x) => Interval x -> Interval x -> Overlap
overlap (orient -> i1) (orient -> i2) =
  case (on compare lower i1 i2, on compare upper i1 i2) of
    (LT, LT) -> case upper i1 `compare` lower i2 of
      LT -> Before
      EQ -> case (imax i1, imin i2) of
        (Just _, Just _) -> Meets
        _ -> Before
      GT -> Overlaps
    (LT, EQ) -> Finishes
    (LT, GT) -> Contains
    (EQ, LT) -> Starts
    (EQ, EQ) -> Identical
    (EQ, GT) -> StartedBy
    (GT, LT) -> During
    (GT, EQ) -> FinishedBy
    (GT, GT) -> case upper i2 `compare` lower i1 of
      GT -> OverlappedBy
      EQ -> case (imin i1, imax i2) of
        (Just _, Just _) -> MetBy
        _ -> After
      LT -> After

intersect ::
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (Interval x)
intersect (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> Nothing
  Meets -> Just (u1 :|-|: u1)
  Overlaps -> Just j1
  Starts -> Just j1
  During -> Just j1
  Finishes -> Just j1
  Identical -> Just i1
  FinishedBy -> Just j2
  Contains -> Just j2
  StartedBy -> Just j2
  OverlappedBy -> Just j2
  MetBy -> Just (l1 :|-|: l1)
  After -> Nothing
  where
    (l1, lb1) = lower i1
    (l2, lb2) = lower i2
    (u1, ub1) = upper i1
    (u2, ub2) = upper i2
    j1 = case (lb2, ub1) of
      (Infimum, Supremum) -> l2 :<->: u1
      (Infimum, Maximum) -> l2 :<-|: u1
      (Minimum, Supremum) -> l2 :|->: u1
      _ -> l2 :|-|: u1
    j2 = case (lb1, ub2) of
      (Infimum, Supremum) -> l1 :<->: u2
      (Infimum, Maximum) -> l1 :<-|: u2
      (Minimum, Supremum) -> l1 :|->: u2
      _ -> l1 :|-|: u2

-- >>> union (2 :||: 5) (5 :<>: 7)
-- One (Merid 2 :|->: Merid 7)
--
-- >>> union (2 :||: 4) (5 :<>: 7)
-- Two (Merid 2 :|-|: Merid 4) (Merid 5 :<->: Merid 7)
union ::
  (Ord x) =>
  Interval x ->
  Interval x ->
  OneOrTwo (Interval x)
union (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> Two i1 i2
  After -> Two i2 i1
  _ -> One (hull i1 i2)

unions :: (Ord x) => [Interval x] -> [Interval x]
unions = foldr f []
  where
    f i [] = [i]
    f i (j : js) = case i `union` j of
      One i' -> f i' js
      _ -> j : f i js

-- >>> complement (3 :<>: 4)
-- Just (Two (Nadir :|-|: Merid 3) (Merid 4 :|-|: Zenit))
complement :: (Ord x) => Interval x -> Maybe (OneOrTwo (Interval x))
complement =
  orient >>> \case
    Nadir :|-|: Zenit -> Nothing
    Nadir :|-|: u -> Just (One (u :<-|: Zenit))
    Nadir :|->: u -> Just (One (u :|-|: Zenit))
    Nadir :<-|: u -> Just (Two (Nadir :|-|: Nadir) (u :<-|: Zenit))
    Nadir :<->: u -> Just (Two (Nadir :|-|: Nadir) (u :|-|: Zenit))
    l :|-|: Zenit -> Just (One (Nadir :|->: l))
    l :<-|: Zenit -> Just (One (Nadir :|-|: l))
    l :|->: Zenit -> Just (Two (Nadir :|->: l) (Zenit :|-|: Zenit))
    l :<->: Zenit -> Just (Two (Nadir :|-|: l) (Zenit :|-|: Zenit))
    l :|-|: u -> Just (Two (Nadir :|->: l) (u :<-|: Zenit))
    l :|->: u -> Just (Two (Nadir :|->: l) (u :|-|: Zenit))
    l :<-|: u -> Just (Two (Nadir :|-|: l) (u :<-|: Zenit))
    l :<->: u -> Just (Two (Nadir :|-|: l) (u :|-|: Zenit))

difference ::
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
difference (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> Just (One i1)
  Meets -> Just . One $ case lb1 of
    Infimum -> l1 :<->: u1
    _ -> l1 :|->: u1
  Overlaps -> Just (One j1)
  Starts -> Nothing
  During -> Nothing
  Finishes -> Nothing
  Identical -> Nothing
  FinishedBy -> Just (One j1)
  Contains -> Just (Two j1 j2)
  StartedBy -> Just (One j2)
  OverlappedBy -> Just (One j2)
  MetBy -> Just . One $ case ub1 of
    Supremum -> l1 :<->: u1
    _ -> l1 :<-|: u1
  After -> Just (One i1)
  where
    (l1, lb1) = lower i1
    (l2, lb2) = lower i2
    (u1, ub1) = upper i1
    (u2, ub2) = upper i2
    j1 = case (lb1, lb2) of
      (Infimum, Infimum) -> l1 :<-|: l2
      (Infimum, _) -> l1 :<->: l2
      (_, Infimum) -> l1 :|-|: l2
      _ -> l1 :|->: l2
    j2 = case (ub2, ub1) of
      (Maximum, Maximum) -> u2 :<-|: u1
      (Maximum, _) -> u2 :<->: u1
      (_, Maximum) -> u2 :|-|: u1
      _ -> u2 :|->: u1

symmetricDifference ::
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
symmetricDifference (orient -> i1) (orient -> i2) = case i1 `union` i2 of
  Two j1 j2 -> Just (Two j1 j2)
  One u -> difference u =<< intersect i1 i2

-- >>> measure (-1 :<>: 1)
-- Just 2
measure :: (Ord x, Num x) => Interval x -> Maybe x
measure = measuring subtract

-- >>> measuring max (-1 :<>: 1)
-- Just 1
measuring :: (Ord x, Num y) => (x -> x -> y) -> Interval x -> Maybe y
measuring f =
  orient >>> \case
    Merid l :|-|: Merid u -> Just (f l u)
    Merid l :|->: Merid u -> Just (f l u)
    Merid l :<-|: Merid u -> Just (f l u)
    Merid l :<->: Merid u -> Just (f l u)
    l :|-|: u -> if l == u then Just 0 else Nothing
    l :|->: u -> if l == u then Just 0 else Nothing
    l :<-|: u -> if l == u then Just 0 else Nothing
    l :<->: u -> if l == u then Just 0 else Nothing

-- >>> hausdorff (3 :<>: 5) (6 :<>: 7)
-- Just 1
hausdorff :: (Ord x, Num x) => Interval x -> Interval x -> Maybe x
hausdorff (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> case (upper i1, lower i2) of
    ((Merid u1, _), (Merid l2, _)) -> Just (l2 - u1)
    _ -> Nothing
  After -> case (lower i1, upper i2) of
    ((Merid l1, _), (Merid u2, _)) -> Just (l1 - u2)
    _ -> Nothing
  _ -> Just 0

-- | @m '+/-' r@ creates the closed interval centred at @m@ with radius @r@.
(+/-) :: (Ord x, Num x) => x -> x -> Interval x
m +/- r = m - r :||: m + r
