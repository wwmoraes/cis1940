module LibSpec (spec) where

import Lib
import Test.Hspec

spec :: Spec
spec = do
  describe "toDigits" $ do
    context "when provided with a positive number" $ do
      it "returns number as digits" $ do
        toDigits 1234 `shouldBe` ([1, 2, 3, 4] :: [Integer])
    context "when provided with zero" $ do
      it "returns an empty list" $ do
        toDigits 0 `shouldBe` ([] :: [Integer])
    context "when provided with a negative number" $ do
      it "returns an empty list" $ do
        toDigits (-1) `shouldBe` ([] :: [Integer])
  describe "toDigitsRev" $ do
    context "when provided with a positive number" $ do
      it "returns number as digits" $ do
        toDigitsRev 1234 `shouldBe` ([4, 3, 2, 1] :: [Integer])
    context "when provided with zero" $ do
      it "returns an empty list" $ do
        toDigitsRev 0 `shouldBe` ([] :: [Integer])
    context "when provided with a negative number" $ do
      it "returns an empty list" $ do
        toDigitsRev (-1) `shouldBe` ([] :: [Integer])
  describe "doubleEveryOther" $ do
    it "returns every second-from-right numbers doubled (even list)" $ do
      doubleEveryOther [3, 4, 5, 6] `shouldBe` ([6, 4, 10, 6] :: [Integer])
    it "returns every second-from-right numbers doubled (odd list)" $ do
      doubleEveryOther [3, 4, 5] `shouldBe` ([3, 8, 5] :: [Integer])
    it "returns single element unmodified" $ do
      doubleEveryOther [3] `shouldBe` ([3] :: [Integer])
    it "returns empty list for no digits" $ do
      doubleEveryOther [] `shouldBe` ([] :: [Integer])
  describe "doubleEveryOtherRev" $ do
    it "returns every second-from-left numbers doubled (even list)" $ do
      doubleEveryOtherRev [3, 4, 5, 6] `shouldBe` ([3, 8, 5, 12] :: [Integer])
    it "returns every second-from-left numbers doubled (odd list)" $ do
      doubleEveryOtherRev [3, 4, 5] `shouldBe` ([3, 8, 5] :: [Integer])
    it "returns single element unmodified" $ do
      doubleEveryOther [3] `shouldBe` ([3] :: [Integer])
    it "returns empty list for no digits" $ do
      doubleEveryOther [] `shouldBe` ([] :: [Integer])
  describe "sumDigits" $ do
    it "returns the sum of all digits" $ do
      sumDigits [16, 7, 12, 5] `shouldBe` (22 :: Integer)
    it "returns zero for empty lists" $ do
      sumDigits [] `shouldBe` (0 :: Integer)
  describe "validate" $ do
    it "should return true" $ do
      validate 4012888888881881 `shouldBe` (True :: Bool)
    it "returns zero for empty lists" $ do
      validate 4012888888881882 `shouldBe` (False :: Bool)
  describe "hanoi" $ do
    context "with zero disks" $ do
      it "should return an empty list" $ do
        hanoi 0 "a" "b" "c" `shouldBe` []
    context "with more than zero disks" $ do
      it "should solve 2 disks properly" $ do
        hanoi 2 "a" "b" "c" `shouldBe` ([("a", "c"), ("a", "b"), ("c", "b")] :: [Move])
      it "should solve 3 disks properly" $ do
        hanoi 3 "a" "b" "c" `shouldBe` ([("a", "b"), ("a", "c"), ("b", "c"), ("a", "b"), ("c", "a"), ("c", "b"), ("a", "b")] :: [Move])
  describe "hanoi4" $ do
    it "should solve properly" $ do
      pendingWith "develop function"
