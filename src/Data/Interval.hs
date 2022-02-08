{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilyDependencies #-}

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
    unBound,
    Oppose (..),
    compareBounds,
    SomeBound (..),
    oppose,
    Interval (..),
    bounds,
    withBounds,
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
    lowerBound,
    upper,
    upperBound,
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
    overlap,
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
    module Data.Interval.Overlap,
  )
where

import Data.Data (Data)
import Data.Interval.Overlap
import Data.OneOrTwo (OneOrTwo (..))
import Data.Suspension (Suspension (..))
import Data.UpToThree
import GHC.Show qualified (show)

-- | The kinds of extremum an interval can have.
data Extremum
  = Minimum
  | Infimum
  | Supremum
  | Maximum
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Data, Typeable)

-- | The 'opposite' of an extremum is how it would be viewed
-- from the other "direction" of how it is currently.
--
-- c.f. 'opposeBound'.
opposite :: Extremum -> Extremum
opposite = \case
  Minimum -> Supremum
  Infimum -> Maximum
  Supremum -> Minimum
  Maximum -> Infimum

-- | A 'Bound' is an endpoint of an 'Interval'.
type Bound :: Extremum -> Type -> Type
data Bound ext x where
  Min :: x -> Bound Minimum x
  Inf :: x -> Bound Infimum x
  Sup :: x -> Bound Supremum x
  Max :: x -> Bound Maximum x

-- | Extract the term from a 'Bound'.
unBound :: Bound ext x -> x
unBound = \case
  Min x -> x
  Inf x -> x
  Sup x -> x
  Max x -> x

instance Functor (Bound ext) where
  fmap f = \case
    Min x -> Min (f x)
    Inf x -> Inf (f x)
    Sup x -> Sup (f x)
    Max x -> Max (f x)

instance Foldable (Bound ext) where
  foldMap f = \case
    Min x -> f x
    Inf x -> f x
    Sup x -> f x
    Max x -> f x

instance Traversable (Bound ext) where
  traverse f = \case
    Min x -> Min <$> f x
    Inf x -> Inf <$> f x
    Sup x -> Sup <$> f x
    Max x -> Max <$> f x

instance (Eq x) => Eq (Bound ext x) where
  Min x == Min y = x == y
  Inf x == Inf y = x == y
  Sup x == Sup y = x == y
  Max x == Max y = x == y

-- | A type class for inverting 'Bound's.
type Oppose :: Extremum -> Constraint
class Oppose ext where
  type Opposite ext :: Extremum

  -- | c.f. 'opposite'.
  opposeBound :: Bound ext x -> Bound (Opposite ext) x

instance Oppose Minimum where
  type Opposite Minimum = Supremum
  opposeBound (Min x) = Sup x

instance Oppose Infimum where
  type Opposite Infimum = Maximum
  opposeBound (Inf x) = Max x

instance Oppose Supremum where
  type Opposite Supremum = Minimum
  opposeBound (Sup x) = Min x

instance Oppose Maximum where
  type Opposite Maximum = Infimum
  opposeBound (Max x) = Inf x

-- | 'Bound's have special comparison rules for identical points.
--
-- - minima are lesser than infima
-- - suprema are lesser than maxima
-- - infima and minima are both lesser than suprema and maxima
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
  -- Open-open interval. You probably want '(:<->:)' or '(:<>:)'.
  (:<-->:) ::
    Bound Infimum (Suspension x) ->
    Bound Supremum (Suspension x) ->
    Interval x
  -- Open-closed interval. You probably want '(:<-|:)' or '(:<|:)'.
  (:<--|:) ::
    Bound Infimum (Suspension x) ->
    Bound Maximum (Suspension x) ->
    Interval x
  -- Closed-open interval. You probably want '(:|->:)' or '(:|>:)'.
  (:|-->:) ::
    Bound Minimum (Suspension x) ->
    Bound Supremum (Suspension x) ->
    Interval x
  -- Closed-closed interval. You probably want '(:|-|:)' or '(:||:)'.
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
pattern (:<>:) :: x -> x -> Interval x
pattern l :<>: u = Merid l :<->: Merid u

-- | A pattern synonym matching finite open-closed intervals.
pattern (:<|:) :: x -> x -> Interval x
pattern l :<|: u = Merid l :<-|: Merid u

-- | A pattern synonym matching finite closed-open intervals.
pattern (:|>:) :: x -> x -> Interval x
pattern l :|>: u = Merid l :|->: Merid u

-- | A pattern synonym matching finite closed intervals.
pattern (:||:) :: x -> x -> Interval x
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

-- | Order properly the endpoints of an interval.
orient :: (Ord x) => Interval x -> Interval x
orient = \case
  l :<->: u
    | l == u -> l :|-|: u
    | otherwise -> min l u :<->: max l u
  l :<-|: u -> min l u :<-|: max l u
  l :|->: u -> min l u :|->: max l u
  l :|-|: u -> min l u :|-|: max l u

data SomeBound x
  = forall ext.
    ( Oppose ext,
      Oppose (Opposite ext),
      Opposite (Opposite ext) ~ ext
    ) =>
    SomeBound (Bound ext x)

oppose :: SomeBound x -> SomeBound x
oppose (SomeBound b) = SomeBound (opposeBound b)

bounds :: Interval x -> (SomeBound (Suspension x), SomeBound (Suspension x))
bounds = \case
  l :<-->: u -> (SomeBound l, SomeBound u)
  l :<--|: u -> (SomeBound l, SomeBound u)
  l :|-->: u -> (SomeBound l, SomeBound u)
  l :|--|: u -> (SomeBound l, SomeBound u)

withBounds ::
  (Ord x) =>
  SomeBound (Suspension x) ->
  SomeBound (Suspension x) ->
  Interval x
withBounds (SomeBound b1) (SomeBound b2) = orient $ case (b1, b2) of
  (Min l, Max u) -> l :|-|: u
  (Inf l, Max u) -> l :<-|: u
  (Min l, Sup u) -> l :|->: u
  (Inf l, Sup u) -> l :<->: u
  (Max u, Min l) -> l :|-|: u
  (Max u, Inf l) -> l :<-|: u
  (Sup u, Min l) -> l :|->: u
  (Sup u, Inf l) -> l :<->: u
  _ -> error "cannot make an interval with the given bounds"

-- | Get the lower bound of an interval
-- (with the bound expressed at the term level).
lower :: (Ord x) => Interval x -> (Suspension x, Extremum)
lower =
  orient >>> \case
    l :<->: _ -> (l, Infimum)
    l :<-|: _ -> (l, Infimum)
    l :|->: _ -> (l, Minimum)
    l :|-|: _ -> (l, Minimum)

lowerBound :: (Ord x) => Interval x -> SomeBound (Suspension x)
lowerBound =
  orient >>> \case
    l :<->: _ -> SomeBound (Inf l)
    l :<-|: _ -> SomeBound (Inf l)
    l :|->: _ -> SomeBound (Min l)
    l :|-|: _ -> SomeBound (Min l)

-- | Get the upper bound of an interval
-- (with the bound expressed at the term level).
upper :: (Ord x) => Interval x -> (Suspension x, Extremum)
upper =
  orient >>> \case
    _ :<->: u -> (u, Supremum)
    _ :<-|: u -> (u, Maximum)
    _ :|->: u -> (u, Supremum)
    _ :|-|: u -> (u, Maximum)

upperBound :: (Ord x) => Interval x -> SomeBound (Suspension x)
upperBound =
  orient >>> \case
    _ :<->: u -> SomeBound (Sup u)
    _ :<-|: u -> SomeBound (Max u)
    _ :|->: u -> SomeBound (Sup u)
    _ :|-|: u -> SomeBound (Max u)

-- | Get the convex hull of two intervals.
hull :: (Ord x) => Interval x -> Interval x -> Interval x
hull (orient -> i1) (orient -> i2) =
  case (on min lower i1 i2, on max upper i1 i2) of
    ((l, Infimum), (u, Supremum)) -> l :<->: u
    ((l, Infimum), (u, _)) -> l :<-|: u
    ((l, _), (u, Supremum)) -> l :|->: u
    ((l, _), (u, _)) -> l :|-|: u

-- | Get the convex hull of a non-empty list of intervals.
hulls :: (Ord x) => NonEmpty (Interval x) -> Interval x
hulls (i :| []) = i
hulls (i :| j : is) = hulls $ hull i j :| is

-- | Test whether a point is contained in the interval.
within :: (Ord x) => x -> Interval x -> Bool
within (Merid -> x) =
  orient >>> \case
    l :<->: u -> l < x && x < u
    l :<-|: u -> l < x && x <= u
    l :|->: u -> l <= x && x < u
    l :|-|: u -> l <= x && x <= u

-- | Create the closed-closed interval at a given point.
point :: x -> Interval x
point = join (:||:)

-- | The whole interval.
pattern Whole :: Interval x
pattern Whole = Nadir :|-|: Zenit

instance (Ord x) => Ord (Interval x) where
  compare i1 i2 = on compare lower i1 i2 <> on compare upper i1 i2

-- | Get the minimum of an interval, if it exists.
imin :: (Ord x) => Interval x -> Maybe (Bound Minimum (Suspension x))
imin =
  orient >>> \case
    (_ :<-->: _) -> Nothing
    (_ :<--|: _) -> Nothing
    (x :|-->: _) -> Just x
    (x :|--|: _) -> Just x

-- | Get the infimum of an interval, weakening if necessary.
iinf :: (Ord x) => Interval x -> Bound Infimum (Suspension x)
iinf =
  orient >>> \case
    (x :<->: _) -> Inf x
    (x :<-|: _) -> Inf x
    (x :|->: _) -> Inf x
    (x :|-|: _) -> Inf x

-- | Get the supremum of an interval, weakening if necessary.
isup :: (Ord x) => Interval x -> Bound Supremum (Suspension x)
isup =
  orient >>> \case
    (_ :<->: x) -> Sup x
    (_ :<-|: x) -> Sup x
    (_ :|->: x) -> Sup x
    (_ :|-|: x) -> Sup x

-- | Get the maximum of an interval if it exists.
imax :: (Ord x) => Interval x -> Maybe (Bound Maximum (Suspension x))
imax =
  orient >>> \case
    (_ :<-->: _) -> Nothing
    (_ :<--|: x) -> Just x
    (_ :|-->: _) -> Nothing
    (_ :|--|: x) -> Just x

-- | Open both bounds of the given interval.
open :: (Ord x) => Interval x -> Interval x
open =
  orient >>> \case
    l :<->: u -> l :<->: u
    l :<-|: u -> l :<->: u
    l :|->: u -> l :<->: u
    l :|-|: u -> l :<->: u

-- | Close both bounds of the given interval.
close :: (Ord x) => Interval x -> Interval x
close =
  orient >>> \case
    l :<->: u -> l :|-|: u
    l :<-|: u -> l :|-|: u
    l :|->: u -> l :|-|: u
    l :|-|: u -> l :|-|: u

-- | Make the interval open-closed, leaving the endpoints unchanged.
openclosed :: (Ord x) => Interval x -> Interval x
openclosed =
  orient >>> \case
    l :<->: u -> l :<->: u
    l :<-|: u -> l :<->: u
    l :|->: u -> l :<->: u
    l :|-|: u -> l :<->: u

-- | Make the interval closed-open, leaving the endpoints unchanged.
closedopen :: (Ord x) => Interval x -> Interval x
closedopen =
  orient >>> \case
    l :<->: u -> l :|-|: u
    l :<-|: u -> l :|-|: u
    l :|->: u -> l :|-|: u
    l :|-|: u -> l :|-|: u

-- | Make the lower bound open, leaving the endpoints unchanged.
openLower :: (Ord x) => Interval x -> Interval x
openLower =
  orient >>> \case
    l :<->: u -> l :<->: u
    l :<-|: u -> l :<-|: u
    l :|->: u -> l :<->: u
    l :|-|: u -> l :<-|: u

-- | Make the lower bound closed, leaving the endpoints unchanged.
closedLower :: (Ord x) => Interval x -> Interval x
closedLower =
  orient >>> \case
    l :<->: u -> l :|->: u
    l :<-|: u -> l :|-|: u
    l :|->: u -> l :|->: u
    l :|-|: u -> l :|-|: u

-- | Make the upper bound open, leaving the endpoints unchanged.
openUpper :: (Ord x) => Interval x -> Interval x
openUpper =
  orient >>> \case
    l :<->: u -> l :<->: u
    l :<-|: u -> l :<->: u
    l :|->: u -> l :|->: u
    l :|-|: u -> l :|->: u

-- | Make the upper bound closed, leaving the endpoints unchanged.
closedUpper :: (Ord x) => Interval x -> Interval x
closedUpper =
  orient >>> \case
    l :<->: u -> l :<-|: u
    l :<-|: u -> l :<-|: u
    l :|->: u -> l :|-|: u
    l :|-|: u -> l :|-|: u

setLower :: (Ord x) => Suspension x -> Interval x -> Interval x
setLower x =
  orient >>> \case
    _ :<->: u -> x :<->: u
    _ :<-|: u -> x :<-|: u
    _ :|->: u -> x :|->: u
    _ :|-|: u -> x :|-|: u

setUpper :: (Ord x) => Suspension x -> Interval x -> Interval x
setUpper x =
  orient >>> \case
    l :<->: _ -> l :<->: x
    l :<-|: _ -> l :<-|: x
    l :|->: _ -> l :|->: x
    l :|-|: _ -> l :|-|: x

-- | Calculate the overlap relationship between two intervals, according to
-- [Allen](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra).
overlap :: (Ord x) => Interval x -> Interval x -> Overlap
overlap (orient -> i1) (orient -> i2) =
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
split :: (Ord x) => Interval x -> Interval x -> UpToThree (Interval x)
split (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> Double i1 i2
  Meets ->
    Triple
      (openUpper i1)
      (withBounds lb2 ub1)
      (openLower i2)
  Overlaps ->
    Triple
      (withBounds lb1 (oppose lb2))
      (withBounds lb2 ub1)
      (withBounds (oppose ub1) ub2)
  Starts -> Double i1 (withBounds (oppose ub1) ub2)
  During ->
    Triple
      (withBounds lb2 (oppose lb1))
      (withBounds lb1 ub1)
      (withBounds (oppose ub1) ub2)
  Finishes -> Double (withBounds lb1 (oppose lb2)) i2
  Identical -> Single i1
  FinishedBy -> Double (withBounds lb2 (oppose lb1)) i1
  Contains ->
    Triple
      (withBounds lb1 (oppose lb2))
      (withBounds lb2 ub2)
      (withBounds (oppose ub2) ub1)
  StartedBy -> Double i2 (withBounds (oppose ub2) ub1)
  OverlappedBy ->
    Triple
      (withBounds lb2 (oppose lb1))
      (withBounds lb1 ub2)
      (withBounds (oppose ub2) ub1)
  MetBy ->
    Triple
      (openUpper i2)
      (withBounds lb1 ub2)
      (openLower i1)
  After -> Double i2 i1
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
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (Interval x)
intersect (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before -> Nothing
  Meets -> Just (u1 :|-|: u1)
  Overlaps -> Just j1
  Starts -> Just j1
  During -> Just i1
  Finishes -> Just j1
  Identical -> Just i1
  FinishedBy -> Just j2
  Contains -> Just i2
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
  (Ord x) =>
  Interval x ->
  Interval x ->
  OneOrTwo (Interval x)
union (orient -> i1) (orient -> i2) = case overlap i1 i2 of
  Before
    | u1 == l2 -> case (ub1, lb2) of
      (Supremum, Infimum) -> Two i1 i2
      _ -> One (hull i1 i2)
    | otherwise -> Two i1 i2
  After
    | u2 == l1 -> case (ub2, lb1) of
      (Supremum, Infimum) -> Two i2 i1
      _ -> One (hull i1 i2)
    | otherwise -> Two i2 i1
  _ -> One (hull i1 i2)
  where
    (l1, lb1) = lower i1
    (l2, lb2) = lower i2
    (u1, ub1) = upper i1
    (u2, ub2) = upper i2

-- | Get the union of a list of intervals.
unions :: (Ord x) => [Interval x] -> [Interval x]
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
complement :: (Ord x) => Interval x -> Maybe (OneOrTwo (Interval x))
complement =
  orient >>> \case
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
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
symmetricDifference (orient -> i1) (orient -> i2) = case i1 `union` i2 of
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
measure :: (Ord x, Num x) => Interval x -> Maybe x
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

-- | Get the distance between two intervals, or 0 if they overlap.
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
hausdorff (orient -> i1) (orient -> i2) = case overlap i1 i2 of
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
