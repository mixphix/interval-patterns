module Data.Interval.Types
  ( Extremum (..),
    opposite,
    Bound (..),
    unBound,
    Oppose (..),
    compareBounds,
    SomeBound (..),
    oppose,
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
    bounds,
    withBounds,
    lower,
    lowerBound,
    upper,
    upperBound,
    orient,
  )
where

import Data.Data (Data)
import Data.Suspension (Suspension (..))
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

-- | The whole interval.
pattern Whole :: Interval x
pattern Whole = Nadir :|-|: Zenit

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

instance (Ord x) => Ord (Interval x) where
  compare i1 i2 = on compare lower i1 i2 <> on compare upper i1 i2

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

-- | Flip an 'Interval' to the direction of the canonical ordering.
--
-- @
--
-- >>> orient (Merid 6 :|->: Nadir)
-- (Nadir :<-|: Merid 6)
--
-- >>> orient (5 :<|: 4)
-- (Merid 4 :|->: Merid 5)
--
-- @
orient :: forall x. (Ord x) => Interval x -> Interval x
orient = \case
  l :<->: u
    | l == u -> l :|-|: u
    | otherwise -> min l u :<->: max l u
  l :<-|: u -> min l u :|->: max l u
  l :|->: u -> min l u :<-|: max l u
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
