-- |
-- Module       : Data.Interval.Adjacency
-- Copyright    : (c) Melanie Brown 2021
-- License:     : BSD3 (see the file LICENSE)
--
-- Allen's interval algebra.
module Data.Interval.Adjacency
  ( Adjacency (..),
    converseAdjacency,
  )
where

import Data.Data (Data)

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
