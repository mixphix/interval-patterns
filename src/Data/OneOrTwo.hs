module Data.OneOrTwo
  ( OneOrTwo (..),
    oneOrTwo,
  )
where

import Data.Data (Data)

-- | Either one of something, or two of it.
--
-- Use 'oneOrTwo' to deconstruct.
data OneOrTwo x
  = One !x
  | Two !x !x
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

-- | Apply a 'oneOrTwo' argument function appropriately.
oneOrTwo :: (x -> a) -> (x -> x -> a) -> OneOrTwo x -> a
oneOrTwo f g = \case
  One x -> f x
  Two x y -> g x y
{-# INLINE oneOrTwo #-}
