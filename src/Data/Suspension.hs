module Data.Suspension where

import Data.Data

data Suspension a
  = Nadir
  | Merid a
  | Zenit
  deriving (Eq, Ord, Show, Read, Generic, Data, Typeable, Functor)

instance Applicative Suspension where
  pure = Merid

  Nadir <*> _ = Nadir
  Merid f <*> x = fmap f x
  Zenit <*> _ = Zenit

instance Monad Suspension where
  Nadir >>= _ = Nadir
  Merid x >>= f = f x
  Zenit >>= _ = Zenit

instance (Semigroup x) => Semigroup (Suspension x) where
  Nadir <> x = x
  Zenit <> _ = Zenit
  x <> Nadir = x
  _ <> Zenit = Zenit
  Merid x <> Merid y = Merid (x <> y)

instance (Semigroup x) => Monoid (Suspension x) where
  mempty = Nadir

suspension :: b -> (a -> b) -> b -> Suspension a -> b
suspension nadir merid zenit = \case
  Nadir -> nadir
  Merid a -> merid a
  Zenit -> zenit

withMerid :: (a -> b) -> Suspension a -> Maybe b
withMerid f = suspension Nothing (Just . f) Nothing

whenMerid :: (Alternative m) => Suspension a -> (a -> m ()) -> m ()
whenMerid s f = suspension pass f pass s
