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
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "smart constructors" $ do
    it "orients finite intervals" $ do
      property @(Int -> Int -> _) $ \x y -> do
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

    it "orients infinite intervals" $ do
      (Top :<->: Bottom) `shouldBe` (Bottom :<->: Top :: Interval Int)
      (Top :|->: Bottom) `shouldBe` (Bottom :<-|: Top :: Interval Int)
      (Top :<-|: Bottom) `shouldBe` (Bottom :|->: Top :: Interval Int)
      (Top :|-|: Bottom) `shouldBe` (Bottom :|-|: Top :: Interval Int)

    it "closes point intervals" $ do
      property @(Int -> _) $ \x -> do
        (x :<>: x) `shouldBe` (x :||: x)
        (x :|>: x) `shouldBe` (x :||: x)
        (x :<|: x) `shouldBe` (x :||: x)
        (x :||: x) `shouldBe` (x :||: x)
        (Levitate x :<->: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :|->: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :<-|: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)
        (Levitate x :|-|: Levitate x) `shouldBe` (Levitate x :|-|: Levitate x)

  describe "Borel intervals" $ do
    it "(<>) is commutative" $ do
      property @(Int -> Int -> Int -> Int -> _) $ \a b x y -> do
        let abxy = Borel.singleton (a :<>: b) <> Borel.singleton (x :<>: y)
            xyab = Borel.singleton (x :<>: y) <> Borel.singleton (a :<>: b)
        abxy `shouldBe` xyab
    it "(<>) is associative" $ do
      property @(Int -> Int -> Int -> Int -> Int -> Int -> _) $ \a b m n x y -> do
        let ab = Borel.singleton (a :<>: b)
            mn = Borel.singleton (m :<>: n)
            xy = Borel.singleton (x :<>: y)
        (ab <> mn) <> xy `shouldBe` ab <> (mn <> xy)
