module Data.OneOrTwo where

import Data.Data (Data)

data OneOrTwo x
  = One x
  | Two x x
  deriving (Eq, Ord, Show, Read, Generic, Data, Typeable)

oneOrTwo :: (x -> a) -> (x -> x -> a) -> OneOrTwo x -> a
oneOrTwo f g = \case
  One x -> f x
  Two x y -> g x y
