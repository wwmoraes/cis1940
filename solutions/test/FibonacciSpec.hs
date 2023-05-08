module FibonacciSpec (spec) where

import Control.Arrow (Arrow (first))
import Fibonacci
import Test.Hspec

spec :: Spec
spec = do
  let
    checks = [(0, 0), (1, 1), (6, 8), (9, 34)] :: [(Integer, Integer)]
    wantFib = [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181] :: [Integer]
  describe "fib" $ do
    it "returns the proper result" $ do
      foldMap (uncurry shouldBe . first fib) checks
  describe "fibs1" $ do
    it "returns the proper results" $ do
      take 20 fibs1 `shouldBe` wantFib
  describe "fibs2" $ do
    it "returns the proper results" $ do
      take 20 fibs2 `shouldBe` wantFib
  describe "nats" $ do
    let wantNats = [1..20]
    it "returns the first 20 natural numbers" $ do
      take 20 (streamToList nats) `shouldBe` wantNats
    it "is shown" $ do
      show nats `shouldBe` show wantNats
  describe "ruler" $ do
    let wantRuler = [0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, 0, 1, 0, 2]
    it "returns the first 20 ruler numbers" $ do
      take 20 (streamToList ruler) `shouldBe` wantRuler
    it "is shown" $ do
      show ruler `shouldBe` show wantRuler
