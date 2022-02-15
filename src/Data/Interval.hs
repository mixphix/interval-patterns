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
    Bounding (..),
    compareBounds,
    SomeBound (..),
    unSomeBound,
    oppose,
    Interval (..),
    imap,
    imapS,
    itraverse,
    itraverseS,
    pattern (:<->:),
    pattern (:<-|:),
    pattern (:|->:),
    pattern (:|-|:),
    pattern (:<>:),
    pattern (:<|:),
    pattern (:|>:),
    pattern (:||:),
    pattern Whole,
    bounds,
    lower,
    lowerBound,
    upper,
    upperBound,
    interval,
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
    Adjacency (..),
    converseAdjacency,
    adjacency,
    intersect,
    union,
    unions,
    unionsAsc,
    complement,
    difference,
    (\\),
    symmetricDifference,
    measure,
    measuring,
    hausdorff,
    (+/-),
  )
where

import Data.Data (Data)
import Data.OneOrTwo (OneOrTwo (..))
import Data.Suspension (Suspension (..), meridToMaybe)
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
  Min :: !x -> Bound Minimum x
  Inf :: !x -> Bound Infimum x
  Sup :: !x -> Bound Supremum x
  Max :: !x -> Bound Maximum x

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

instance (Ord x) => Ord (Bound ext (Suspension x)) where
  compare = compareBounds

-- | A type class for inverting 'Bound's.
type Bounding :: Extremum -> Constraint
class
  ( Opposite (Opposite ext) ~ ext
  ) =>
  Bounding ext
  where
  type Opposite ext :: Extremum
  bound :: x -> Bound ext x

  -- | c.f. 'opposite'.
  opposeBound :: Bound ext x -> Bound (Opposite ext) x

instance Bounding Minimum where
  type Opposite Minimum = Supremum
  bound = Min
  opposeBound (Min x) = Sup x

instance Bounding Infimum where
  type Opposite Infimum = Maximum
  bound = Inf
  opposeBound (Inf x) = Max x

instance Bounding Supremum where
  type Opposite Supremum = Minimum
  bound = Sup
  opposeBound (Sup x) = Min x

instance Bounding Maximum where
  type Opposite Maximum = Infimum
  bound = Max
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

data SomeBound x
  = forall ext.
    (Bounding ext, Bounding (Opposite ext)) =>
    SomeBound !(Bound ext x)

instance (Eq x) => Eq (SomeBound (Suspension x)) where
  SomeBound (Min a) == SomeBound (Min b) = a == b
  SomeBound (Max a) == SomeBound (Max b) = a == b
  SomeBound (Inf a) == SomeBound (Inf b) = a == b
  SomeBound (Sup a) == SomeBound (Sup b) = a == b
  _ == _ = False

instance (Ord x) => Ord (SomeBound (Suspension x)) where
  SomeBound b0 `compare` SomeBound b1 = compareBounds b0 b1

oppose :: SomeBound x -> SomeBound x
oppose (SomeBound b) = SomeBound (opposeBound b)

unSomeBound :: (Ord x) => SomeBound x -> x
unSomeBound (SomeBound b) = unBound b

infix 5 :<-->:

infix 5 :<--|:

infix 5 :|-->:

infix 5 :|--|:

type Interval :: Type -> Type
data Interval x where
  -- Open-open interval. You probably want '(:<->:)' or '(:<>:)'.
  (:<-->:) ::
    (Ord x) =>
    !(Bound Infimum (Suspension x)) ->
    !(Bound Supremum (Suspension x)) ->
    Interval x
  -- Open-closed interval. You probably want '(:<-|:)' or '(:<|:)'.
  (:<--|:) ::
    (Ord x) =>
    !(Bound Infimum (Suspension x)) ->
    !(Bound Maximum (Suspension x)) ->
    Interval x
  -- Closed-open interval. You probably want '(:|->:)' or '(:|>:)'.
  (:|-->:) ::
    (Ord x) =>
    !(Bound Minimum (Suspension x)) ->
    !(Bound Supremum (Suspension x)) ->
    Interval x
  -- Closed-closed interval. You probably want '(:|-|:)' or '(:||:)'.
  (:|--|:) ::
    (Ord x) =>
    !(Bound Minimum (Suspension x)) ->
    !(Bound Maximum (Suspension x)) ->
    Interval x

deriving instance (Ord x) => Eq (Interval x)

instance (Ord x, Show x) => Show (Interval x) where
  show = \case
    l :<->: u -> "(" <> show l <> " :<->: " <> show u <> ")"
    l :|->: u -> "(" <> show l <> " :|->: " <> show u <> ")"
    l :<-|: u -> "(" <> show l <> " :<-|: " <> show u <> ")"
    l :|-|: u -> "(" <> show l <> " :|-|: " <> show u <> ")"

instance (Ord x) => Ord (Interval x) where
  compare i1 i2 = on compare lower i1 i2 <> on compare upper i1 i2

-- | Since the 'Ord' constraints on the constructors for 'Interval'
-- prevent it from being a 'Functor', this will have to suffice.
imap :: (Ord x, Ord y) => (x -> y) -> Interval x -> Interval y
imap f = \case
  l :<->: u -> fmap f l :<->: fmap f u
  l :|->: u -> fmap f l :|->: fmap f u
  l :<-|: u -> fmap f l :<-|: fmap f u
  l :|-|: u -> fmap f l :|-|: fmap f u

-- | Same as 'imap' but on the 'Suspension' of the underlying type.
imapS ::
  (Ord x, Ord y) =>
  (Suspension x -> Suspension y) ->
  Interval x ->
  Interval y
imapS f = \case
  l :<->: u -> f l :<->: f u
  l :|->: u -> f l :|->: f u
  l :<-|: u -> f l :<-|: f u
  l :|-|: u -> f l :|-|: f u

-- | Since the 'Ord' constraints on the constructors for 'Interval'
-- prevent it from being 'Traversable', this will have to suffice.
itraverse ::
  (Ord x, Ord y, Applicative f) =>
  (x -> f y) ->
  Interval x ->
  f (Interval y)
itraverse f = \case
  l :<->: u -> liftA2 (:<->:) (traverse f l) (traverse f u)
  l :|->: u -> liftA2 (:|->:) (traverse f l) (traverse f u)
  l :<-|: u -> liftA2 (:<-|:) (traverse f l) (traverse f u)
  l :|-|: u -> liftA2 (:|-|:) (traverse f l) (traverse f u)

-- | Same as 'itraverse' but on the 'Suspension' of the underlying type.
itraverseS ::
  (Ord x, Ord y, Applicative f) =>
  (Suspension x -> f (Suspension y)) ->
  Interval x ->
  f (Interval y)
itraverseS f = \case
  l :<->: u -> liftA2 (:<->:) (f l) (f u)
  l :|->: u -> liftA2 (:|->:) (f l) (f u)
  l :<-|: u -> liftA2 (:<-|:) (f l) (f u)
  l :|-|: u -> liftA2 (:|-|:) (f l) (f u)

infix 5 :<->:

infix 5 :<-|:

infix 5 :|->:

infix 5 :|-|:

-- | A pattern synonym matching open intervals.
pattern (:<->:) :: (Ord x) => Suspension x -> Suspension x -> Interval x
pattern l :<->: u <-
  Inf l :<-->: Sup u
  where
    b1 :<->: b2 = Inf (min b1 b2) :<-->: Sup (max b1 b2)

-- | A pattern synonym matching open-closed intervals.
pattern (:<-|:) :: (Ord x) => Suspension x -> Suspension x -> Interval x
pattern l :<-|: u <-
  Inf l :<--|: Max u
  where
    b1 :<-|: b2 = Inf (min b1 b2) :<--|: Max (max b1 b2)

-- | A pattern synonym matching closed-open intervals.
pattern (:|->:) :: (Ord x) => Suspension x -> Suspension x -> Interval x
pattern l :|->: u <-
  Min l :|-->: Sup u
  where
    b1 :|->: b2 = Min (min b1 b2) :|-->: Sup (max b1 b2)

-- | A pattern synonym matching closed intervals.
pattern (:|-|:) :: (Ord x) => Suspension x -> Suspension x -> Interval x
pattern l :|-|: u <-
  Min l :|--|: Max u
  where
    b1 :|-|: b2 = Min (min b1 b2) :|--|: Max (max b1 b2)

{-# COMPLETE (:<->:), (:<-|:), (:|->:), (:|-|:) #-}

infix 5 :<>:

infix 5 :<|:

infix 5 :|>:

infix 5 :||:

-- | A pattern synonym matching finite open intervals.
pattern (:<>:) :: forall x. (Ord x) => x -> x -> Interval x
pattern l :<>: u = Merid l :<->: Merid u

-- | A pattern synonym matching finite open-closed intervals.
pattern (:<|:) :: forall x. (Ord x) => x -> x -> Interval x
pattern l :<|: u = Merid l :<-|: Merid u

-- | A pattern synonym matching finite closed-open intervals.
pattern (:|>:) :: forall x. (Ord x) => x -> x -> Interval x
pattern l :|>: u = Merid l :|->: Merid u

-- | A pattern synonym matching finite closed intervals.
pattern (:||:) :: forall x. (Ord x) => x -> x -> Interval x
pattern l :||: u = Merid l :|-|: Merid u

-- | The whole interval.
pattern Whole :: (Ord x) => Interval x
pattern Whole = Nadir :|-|: Zenit

-- | Get the @(lower, upper)@ 'bounds' of an 'Interval'.
--
-- c.f. 'lower', 'upper'.
bounds :: Interval x -> (SomeBound (Suspension x), SomeBound (Suspension x))
bounds = \case
  l :<-->: u -> (SomeBound l, SomeBound u)
  l :<--|: u -> (SomeBound l, SomeBound u)
  l :|-->: u -> (SomeBound l, SomeBound u)
  l :|--|: u -> (SomeBound l, SomeBound u)

-- | Get the lower bound of an interval.
--
-- > lower = fst . bounds
lower :: (Ord x) => Interval x -> SomeBound (Suspension x)
lower = fst . bounds

-- | Get the upper bound of an interval.
--
-- > upper = snd . bounds
upper :: (Ord x) => Interval x -> SomeBound (Suspension x)
upper = snd . bounds

-- | Get the lower bound of an interval
-- (with the bound expressed at the term level).
lowerBound :: (Ord x) => Interval x -> (Suspension x, Extremum)
lowerBound = \case
  l :<->: _ -> (l, Infimum)
  l :<-|: _ -> (l, Infimum)
  l :|->: _ -> (l, Minimum)
  l :|-|: _ -> (l, Minimum)

-- | Get the upper bound of an interval
-- (with the bound expressed at the term level).
upperBound :: (Ord x) => Interval x -> (Suspension x, Extremum)
upperBound = \case
  _ :<->: u -> (u, Supremum)
  _ :<-|: u -> (u, Maximum)
  _ :|->: u -> (u, Supremum)
  _ :|-|: u -> (u, Maximum)

-- | Given 'SomeBound's, try to make an interval.
interval ::
  (Ord x) =>
  SomeBound (Suspension x) ->
  SomeBound (Suspension x) ->
  Interval x
interval (SomeBound b1) (SomeBound b2) = case (b1, b2) of
  (Min l, Sup u) -> l :|->: u
  (Min l, Max u) -> l :|-|: u
  (Inf l, Sup u) -> l :<->: u
  (Inf l, Max u) -> l :<-|: u
  (Sup u, Min l) -> l :|->: u
  (Sup u, Inf l) -> l :<->: u
  (Max u, Min l) -> l :|-|: u
  (Max u, Inf l) -> l :<-|: u
  _ -> error "cannot make an interval with the given bounds"

-- | According to
-- [Allen](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra),
-- two intervals can be "adjacent" in 13 different ways,
-- into at most 3 distinct intervals. In this package,
-- this quality is called the 'Adjacency' of the intervals.
data Adjacency x
  = Before !(Interval x) !(Interval x)
  | Meets !(Interval x) !(Interval x) !(Interval x)
  | Overlaps !(Interval x) !(Interval x) !(Interval x)
  | Starts !(Interval x) !(Interval x)
  | During !(Interval x) !(Interval x) !(Interval x)
  | Finishes !(Interval x) !(Interval x)
  | Identical !(Interval x)
  | FinishedBy !(Interval x) !(Interval x)
  | Contains !(Interval x) !(Interval x) !(Interval x)
  | StartedBy !(Interval x) !(Interval x)
  | OverlappedBy !(Interval x) !(Interval x) !(Interval x)
  | MetBy !(Interval x) !(Interval x) !(Interval x)
  | After !(Interval x) !(Interval x)
  deriving (Eq, Ord, Show, Generic, Typeable)

-- | The result of having compared the same two intervals in reverse order.
converseAdjacency :: Adjacency x -> Adjacency x
converseAdjacency = \case
  Before i j -> After i j
  Meets i j k -> MetBy i j k
  Overlaps i j k -> OverlappedBy i j k
  Starts i j -> StartedBy i j
  During i j k -> Contains i j k
  Finishes i j -> FinishedBy i j
  Identical i -> Identical i
  FinishedBy i j -> Finishes i j
  Contains i j k -> During i j k
  StartedBy i j -> Starts i j
  OverlappedBy i j k -> Overlaps i j k
  MetBy i j k -> Meets i j k
  After i j -> Before i j

-- | Get the convex hull of two intervals.
--
-- >>> hull (7 :|>: 8) (3 :|>: 4)
-- (Merid 3 :|->: Merid 8)
--
-- >>> hull (Nadir :<-|: 3) (3 :<|: 4)
-- (Nadir :<-|: Merid 4)
hull :: (Ord x) => Interval x -> Interval x -> Interval x
hull i1 i2 = case (lower (min i1 i2), upper (max i1 i2)) of
  (SomeBound l@(Inf _), SomeBound u@(Sup _)) -> l :<-->: u
  (SomeBound l@(Inf _), SomeBound u@(Max _)) -> l :<--|: u
  (SomeBound l@(Min _), SomeBound u@(Sup _)) -> l :|-->: u
  (SomeBound l@(Min _), SomeBound u@(Max _)) -> l :|--|: u
  _ -> error "Invalid lower/upper bounds"

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

-- | Calculate the 'Adjacency' between two intervals, according to
-- [Allen](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra).
adjacency :: (Ord x) => Interval x -> Interval x -> Adjacency x
adjacency i1 i2 = case (on compare lower i1 i2, on compare upper i1 i2) of
  (LT, LT) -> case unSomeBound ub1 `compare` unSomeBound lb2 of
    LT -> Before i1 i2
    EQ -> case (ub1, lb2) of
      (SomeBound (Max _), SomeBound (Min _)) ->
        Meets
          (openUpper i1)
          (interval lb2 ub1)
          (openLower i2)
      _ -> Before i1 i2
    GT ->
      Overlaps
        (interval lb1 (oppose lb2))
        (interval lb2 ub1)
        (interval (oppose ub1) ub2)
  (LT, EQ) ->
    Finishes
      (interval lb1 (oppose lb2))
      i2
  (LT, GT) ->
    Contains
      (interval lb1 (oppose lb2))
      (interval lb2 ub2)
      (interval (oppose ub2) ub1)
  (EQ, LT) ->
    Starts
      i1
      (interval (oppose ub1) ub2)
  (EQ, EQ) -> Identical i1
  (EQ, GT) ->
    StartedBy
      i2
      (interval (oppose ub2) ub1)
  (GT, LT) ->
    During
      (interval lb2 (oppose lb1))
      (interval lb1 ub1)
      (interval (oppose ub1) ub2)
  (GT, EQ) ->
    FinishedBy
      (interval lb2 (oppose lb1))
      i1
  (GT, GT) -> case unSomeBound ub2 `compare` unSomeBound lb1 of
    GT ->
      OverlappedBy
        (interval lb2 (oppose lb1))
        (interval lb1 ub2)
        (interval (oppose ub2) ub1)
    EQ -> case (ub2, lb1) of
      (SomeBound (Max _), SomeBound (Min _)) ->
        MetBy
          (openUpper i2)
          (interval lb1 ub2)
          (openLower i1)
      _ -> After i2 i1
    LT -> After i2 i1
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
intersect i1 i2 = case adjacency i1 i2 of
  Before _ _ -> Nothing
  Meets _ j _ -> Just j
  Overlaps _ j _ -> Just j
  Starts i _ -> Just i
  During _ j _ -> Just j
  Finishes _ j -> Just j
  Identical i -> Just i
  FinishedBy _ j -> Just j
  Contains _ j _ -> Just j
  StartedBy i _ -> Just i
  OverlappedBy _ j _ -> Just j
  MetBy _ j _ -> Just j
  After _ _ -> Nothing

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
union i1 i2 = case adjacency i1 i2 of
  Before i j
    | fst (upperBound i) == fst (lowerBound j) -> One $ hull i j
    | otherwise -> Two i j
  Meets i j k -> One $ hulls (k :| [hull i j])
  Overlaps i j k -> One $ hulls (i :| [j, k])
  Starts i j -> One $ hulls (i :| [j])
  During i j k -> One $ hulls (i :| [j, k])
  Finishes i j -> One $ hulls (i :| [j])
  Identical i -> One i
  FinishedBy i j -> One $ hulls (i :| [j])
  Contains i j k -> One $ hulls (i :| [j, k])
  StartedBy i j -> One $ hulls (i :| [j])
  OverlappedBy i j k -> One $ hulls (i :| [j, k])
  MetBy i j k -> One $ hulls (k :| [hull i j])
  After i j
    | fst (upperBound i) == fst (lowerBound j) -> One $ hull i j
    | otherwise -> Two i j

-- | /O(n log n)/. Get the union of a list of intervals.
--
-- This function uses 'sort'. See also 'unionsAsc'.
unions :: forall x. (Ord x) => [Interval x] -> [Interval x]
unions = unionsAsc . sort

-- | /O(n)/. Get the union of a sorted list of intervals.
--
-- NOTE: The input condition is not checked. Use with care.
unionsAsc :: forall x. (Ord x) => [Interval x] -> [Interval x]
unionsAsc = \case
  i : j : is -> case i `union` j of
    One k -> unions (k : is)
    _ -> i : unions (j : is)
  x -> x

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
difference i1 i2 = case adjacency i1 i2 of
  -- not commutative!!
  Before i _ -> Just $ One i
  Meets i _ _ -> Just $ One i
  Overlaps i _ _ -> Just $ One i
  Starts {} -> Nothing
  During {} -> Nothing
  Finishes {} -> Nothing
  Identical {} -> Nothing
  FinishedBy i _ -> Just $ One i
  Contains i _ k -> Just $ Two i k
  StartedBy _ j -> Just $ One j
  OverlappedBy _ _ k -> Just $ One k
  MetBy i _ _ -> Just $ One i
  After i _ -> Just $ One i

-- | Infix synonym for 'difference'
(\\) ::
  forall x.
  (Ord x) =>
  Interval x ->
  Interval x ->
  Maybe (OneOrTwo (Interval x))
(\\) = difference

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
  Before i j ->
    meridToMaybe $ on (-) unSomeBound (lower j) (upper i)
  After i j ->
    meridToMaybe $ on (-) unSomeBound (lower j) (upper i)
  _ -> Just 0

-- | @m '+/-' r@ creates the closed interval centred at @m@ with radius @r@.
--
-- For the open interval, simply write @'open' (x '+/-' y)@.
(+/-) :: (Ord x, Num x) => x -> x -> Interval x
m +/- r = m - r :||: m + r
