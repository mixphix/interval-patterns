{-# LANGUAGE UndecidableInstances #-}

module Main where

import Algebra.Lattice.Levitated (Levitated (..))
import Data.Interval (
  Interval,
  pattern (:<->:),
  pattern (:<-|:),
  pattern (:<>:),
  pattern (:<|:),
  pattern (:|->:),
  pattern (:|-|:),
  pattern (:|>:),
  pattern (:||:),
 )
import Data.Interval.Borel qualified as Borel
import Data.Interval.Layers qualified as Layers
import Data.Semigroup
import GHC.TypeNats
import Test.Hspec
import Test.QuickCheck

type family Ints (n :: Nat) x where
  Ints 0 x = x
  Ints n x = Int -> Ints (n - 1) x

main :: IO ()
main = hspec do
  describe "smart constructors" do
    it "orient finite intervals" do
      property @(Ints 2 _) \x y -> do
        if x <= y
          then do
            (x :<>: y) `shouldBe` (x :<>: y)
            (x :|>: y) `shouldBe` (x :|>: y)
            (x :<|: y) `shouldBe` (x :<|: y)
            (x :||: y) `shouldBe` (x :||: y)
            (Levitate x :<->: Levitate y) `shouldBe` (Levitate x :<->: Levitate y)
            (Levitate x :|->: Levitate y) `shouldBe` (Levitate x :|->: Levitate y)
            (Levitate x :<-|: Levitate y) `shouldBe` (Levitate x :<-|: Levitate y)
            (Levitate x :|-|: Levitate y) `shouldBe` (Levitate x :|-|: Levitate y)
          else do
            (x :<>: y) `shouldBe` (y :<>: x)
            (x :|>: y) `shouldBe` (y :<|: x)
            (x :<|: y) `shouldBe` (y :|>: x)
            (x :||: y) `shouldBe` (y :||: x)
            (Levitate x :<->: Levitate y) `shouldBe` (Levitate y :<->: Levitate x)
            (Levitate x :|->: Levitate y) `shouldBe` (Levitate y :<-|: Levitate x)
            (Levitate x :<-|: Levitate y) `shouldBe` (Levitate y :|->: Levitate x)
            (Levitate x :|-|: Levitate y) `shouldBe` (Levitate y :|-|: Levitate x)

    it "orient infinite intervals" do
      (Top :<->: Bottom) `shouldBe` (Bottom :<->: Top :: Interval Int)
      (Top :|->: Bottom) `shouldBe` (Bottom :<-|: Top :: Interval Int)
      (Top :<-|: Bottom) `shouldBe` (Bottom :|->: Top :: Interval Int)
      (Top :|-|: Bottom) `shouldBe` (Bottom :|-|: Top :: Interval Int)

    it "close point intervals" do
      property @(Int -> _) $ \x -> do
        (x :<>: x) `shouldBe` (x :||: x)
        (x :|>: x) `shouldBe` (x :||: x)
        (x :<|: x) `shouldBe` (x :||: x)
        (x :||: x) `shouldBe` (x :||: x)
        (Levitate x :<->: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :|->: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :<-|: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :|-|: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)

  describe "Borel intervals" do
    it "(<>) is commutative" do
      property @(Ints 4 _) \a b x y -> do
        let abxy = Borel.singleton (a :<>: b) <> Borel.singleton (x :<>: y)
            xyab = Borel.singleton (x :<>: y) <> Borel.singleton (a :<>: b)
        abxy `shouldBe` xyab
    it "(<>) is associative" do
      property @(Ints 6 _) \a b m n x y -> do
        let ab = Borel.singleton (a :<>: b)
            mn = Borel.singleton (m :<>: n)
            xy = Borel.singleton (x :<>: y)
        (ab <> mn) <> xy `shouldBe` ab <> (mn <> xy)

  describe "Layers" do
    it "(<>) is associative" do
      property @(Ints 9 _) \a b c d e f x y z -> do
        let abx = Layers.singleton (a :<>: b) (Sum x)
            cdy = Layers.singleton (c :||: d) (Sum y)
            efz = Layers.singleton (e :<>: f) (Sum z)
        (abx <> cdy) <> efz `shouldBe` abx <> (cdy <> efz)
