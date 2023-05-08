{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use -" #-}
{-# HLINT ignore "Evaluate" #-}
module SizedSpec (spec) where

import Data.Foldable  (Foldable (fold))
import Provided.Sized (Size (Size), Sized (size), getSize)
import Test.Hspec

spec :: Spec
spec = do
  describe "Size" $ do
    context "with same values" $ do
      let
        val1 = Size 1
        val2 = Size 1
      it "is equal" $ do
        val1 == val2 `shouldBe` True
      it "is orderable" $ do
        compare val1 val2 `shouldBe` EQ
      it "is shown" $ do
        show val1 `shouldBe` "Size 1"
        show val2 `shouldBe` "Size 1"
    context "with different values" $ do
      let
        val1 = Size 2
        val2 = Size 3
        val3 = Size 4
      it "is different" $ do
        val1 /= val2 `shouldBe` True
        val1 /= val3 `shouldBe` True
        val2 /= val3 `shouldBe` True
      it "is orderable" $ do
        compare val1 val1 `shouldBe` EQ
        compare val1 val2 `shouldBe` LT
        compare val1 val3 `shouldBe` LT
        compare val2 val1 `shouldBe` GT
        compare val2 val2 `shouldBe` EQ
        compare val2 val3 `shouldBe` LT
        compare val3 val1 `shouldBe` GT
        compare val3 val2 `shouldBe` GT
        compare val3 val3 `shouldBe` EQ
        min val1 val2 `shouldBe` val1
        min val1 val3 `shouldBe` val1
        min val2 val1 `shouldBe` val1
        min val2 val3 `shouldBe` val2
        min val3 val1 `shouldBe` val1
        min val3 val2 `shouldBe` val2
        max val1 val2 `shouldBe` val2
        max val1 val3 `shouldBe` val3
        max val2 val1 `shouldBe` val2
        max val2 val3 `shouldBe` val3
        max val3 val1 `shouldBe` val3
        max val3 val2 `shouldBe` val3
      it "has additive associativity" $ do
        (val1 + val2) + val3 == val1 + (val2 + val3) `shouldBe` True
      it "has additive commutativity" $ do
        val1 + val2 == val2 + val1 `shouldBe` True
      it "has additive identity" $ do
        val1 + 0 == val1 `shouldBe` True
      it "has additive inverse" $ do
        val1 + negate val1 == Size 0 `shouldBe` True
      it "has multiplicative associativity" $ do
        (val1 * val2) * val3 == val1 * (val2 * val3) `shouldBe` True
      it "has multiplicative identity" $ do
        val1 * 1 == val1 `shouldBe` True
      it "has multiplicative distributivity" $ do
        val1 * (val2 + val3) == (val1 * val2) + (val1 * val3) `shouldBe` True
        (val2 + val3) * val1 == (val2 * val1) + (val3 * val1) `shouldBe` True
      it "is associative" $ do
        val1 <> val2 <> val3 `shouldBe` Size 9
      it "has an identity value" $ do
        fold [val1, val2, val3] `shouldBe` Size 9
  describe "Sized" $ do
    context "with a Size" $ do
      let val = Size 1
      it "returns itself on size" $ do
        size val `shouldBe` val
      it "returns the value on getSize" $ do
        getSize val `shouldBe` 1
    context "with a tuple" $ do
      let
        val = Size 1
        wrapper = ("foo", val)
      it "returns the size from the second element" $ do
        size wrapper `shouldBe` val
  describe "getSize" $ do
    context "with a plain Size value" $ do
      let val = Size 1
      it "extracts the size number" $ do
        getSize val `shouldBe` 1
    context "with a Sized tuple" $ do
      let val = ("foo", Size 1)
      it "extracts the size number" $ do
        getSize (size val) `shouldBe` 1
