-- |
-- Module       : Data.Interval.Overlap
-- Copyright    : (c) Melanie Brown 2021
-- License:     : BSD3 (see the file LICENSE)
--
-- Allen's interval algebra.
module Data.Interval.Overlap
  ( Overlap (..),
    converseOverlap,
  )
where

import Data.Data (Data)

-- | Allen's Interval algebra
data Overlap
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

converseOverlap :: Overlap -> Overlap
converseOverlap = \case
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
