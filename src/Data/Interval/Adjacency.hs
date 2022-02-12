-- |
-- Module       : Data.Interval.Adjacency
-- Copyright    : (c) Melanie Brown 2021
-- License:     : BSD3 (see the file LICENSE)
--
-- Allen's interval algebra.
module Data.Interval.Adjacency
  ( Adjacency (..),
    converseAdjacency,
    AdjacencyRepr (..),
    KnownAdjacency,
    knownAdjacency,
    Adjacent (..),
    SomeAdjacency (..),
  )
where

import Data.Data (Data)
import Data.Interval.Types
import Data.List qualified as List (unwords)
import Data.Parameterized (KnownRepr (..))
import Data.Parameterized.WithRepr
import GHC.Show qualified (show)

-- | Allen's Interval algebra
data Adjacency
  = Before
  | Meets
  | Overlaps
  | Starts
  | During
  | Finishes
  | Identical
  | FinishedBy
  | Contains
  | StartedBy
  | OverlappedBy
  | MetBy
  | After
  deriving (Eq, Ord, Enum, Bounded, Show, Read, Generic, Data, Typeable)

-- | The result of having compared the same two intervals in reverse order.
converseAdjacency :: Adjacency -> Adjacency
converseAdjacency = \case
  Before -> After
  Meets -> MetBy
  Overlaps -> OverlappedBy
  Starts -> StartedBy
  During -> Contains
  Finishes -> FinishedBy
  Identical -> Identical
  FinishedBy -> Finishes
  Contains -> During
  StartedBy -> Starts
  OverlappedBy -> Overlaps
  MetBy -> Meets
  After -> Before

type AdjacencyRepr :: Adjacency -> Type
data AdjacencyRepr adj where
  BeforeRepr :: AdjacencyRepr Before
  MeetsRepr :: AdjacencyRepr Meets
  OverlapsRepr :: AdjacencyRepr Overlaps
  StartsRepr :: AdjacencyRepr Starts
  DuringRepr :: AdjacencyRepr During
  FinishesRepr :: AdjacencyRepr Finishes
  IdenticalRepr :: AdjacencyRepr Identical
  FinishedByRepr :: AdjacencyRepr FinishedBy
  ContainsRepr :: AdjacencyRepr Contains
  StartedByRepr :: AdjacencyRepr StartedBy
  OverlappedByRepr :: AdjacencyRepr OverlappedBy
  MetByRepr :: AdjacencyRepr MetBy
  AfterRepr :: AdjacencyRepr After

instance IsRepr AdjacencyRepr

instance KnownRepr AdjacencyRepr Before where
  knownRepr = BeforeRepr

instance KnownRepr AdjacencyRepr Meets where
  knownRepr = MeetsRepr

instance KnownRepr AdjacencyRepr Overlaps where
  knownRepr = OverlapsRepr

instance KnownRepr AdjacencyRepr Starts where
  knownRepr = StartsRepr

instance KnownRepr AdjacencyRepr During where
  knownRepr = DuringRepr

instance KnownRepr AdjacencyRepr Finishes where
  knownRepr = FinishesRepr

instance KnownRepr AdjacencyRepr Identical where
  knownRepr = IdenticalRepr

instance KnownRepr AdjacencyRepr FinishedBy where
  knownRepr = FinishedByRepr

instance KnownRepr AdjacencyRepr Contains where
  knownRepr = ContainsRepr

instance KnownRepr AdjacencyRepr StartedBy where
  knownRepr = StartedByRepr

instance KnownRepr AdjacencyRepr OverlappedBy where
  knownRepr = OverlappedByRepr

instance KnownRepr AdjacencyRepr MetBy where
  knownRepr = MetByRepr

instance KnownRepr AdjacencyRepr After where
  knownRepr = AfterRepr

type KnownAdjacency = KnownRepr AdjacencyRepr

knownAdjacency :: (KnownAdjacency adj) => AdjacencyRepr adj
knownAdjacency = knownRepr

type Adjacent :: Adjacency -> Type -> Type
data Adjacent adj x where
  BeforeJ :: (Ord x) => Interval x -> Interval x -> Adjacent Before x
  MeetsJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent Meets x
  OverlapsJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent Overlaps x
  StartsJ :: (Ord x) => Interval x -> Interval x -> Adjacent Starts x
  DuringJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent During x
  FinishesJ :: (Ord x) => Interval x -> Interval x -> Adjacent Finishes x
  IdenticalJ :: (Ord x) => Interval x -> Adjacent Identical x
  FinishedByJ :: (Ord x) => Interval x -> Interval x -> Adjacent FinishedBy x
  ContainsJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent Contains x
  StartedByJ :: (Ord x) => Interval x -> Interval x -> Adjacent StartedBy x
  OverlappedByJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent OverlappedBy x
  MetByJ :: (Ord x) => Interval x -> Interval x -> Interval x -> Adjacent MetBy x
  AfterJ :: (Ord x) => Interval x -> Interval x -> Adjacent After x

instance (Show x) => Show (Adjacent adj x) where
  show =
    List.unwords . \case
      BeforeJ i j -> ["BeforeJ", show i, show j]
      MeetsJ i j k -> ["MeetsJ", show i, show j, show k]
      OverlapsJ i j k -> ["OverlapsJ", show i, show j, show k]
      StartsJ i j -> ["StartsJ", show i, show j]
      DuringJ i j k -> ["DuringJ", show i, show j, show k]
      FinishesJ i j -> ["FinishesJ", show i, show j]
      IdenticalJ i -> ["IdenticalJ", show i]
      FinishedByJ i j -> ["FinishedByJ", show i, show j]
      ContainsJ i j k -> ["ContainsJ", show i, show j, show k]
      StartedByJ i j -> ["StartedByJ", show i, show j]
      OverlappedByJ i j k -> ["OverlappedByJ", show i, show j, show k]
      MetByJ i j k -> ["MetByJ", show i, show j, show k]
      AfterJ i j -> ["AfterJ", show i, show j]

data SomeAdjacency x = forall adj. (KnownAdjacency adj, Ord x) => SomeAdjacency (Adjacent adj x)

instance (Show x) => Show (SomeAdjacency x) where
  show (SomeAdjacency jac) = "SomeAdjacency (" <> show jac <> ")"
