module Data.UpToThree
  ( UpToThree (..),
    upToThree,
  )
where

import Data.Data (Data)

-- | Could be one of something, or two or three of it.
--
-- Use 'upToThree' to deconstruct.
data UpToThree x
  = Uno !x
  | Dos !x !x
  | Tre !x !x !x
  deriving
    ( Eq,
      Ord,
      Show,
      Read,
      Generic,
      Data,
      Typeable,
      Functor,
      Foldable,
      Traversable
    )

-- | Apply an 'upToThree'-argument function appropriately.
upToThree :: (x -> a) -> (x -> x -> a) -> (x -> x -> x -> a) -> UpToThree x -> a
upToThree f g h = \case
  Uno x -> f x
  Dos x y -> g x y
  Tre x y z -> h x y z
{-# INLINE upToThree #-}
