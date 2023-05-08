module HOCSpec (spec) where

import HOC
import Provided.Wholemeal
import Test.Hspec

spec :: Spec
spec = do
  describe "fun1" $ do
    let sample = [2, 4, 7]
    context "with a point-free re-implementation" $ do
      it "should return the same result" $ do
        fun1' sample `shouldBe` fun1 sample
  describe "fun2" $ do
    let sample = 5
    context "with a point-free re-implementation" $ do
      it "should return the same result" $ do
        fun2' sample `shouldBe` fun2 sample
  describe "foldTree" $ do
    let
      reify :: [String] -> [String]
      reify = id
    context "with an empty array" $ do
      let val = reify []
      it "returns a leaf tree" $ do
        show (foldTree val) `shouldBe` "Leaf"
    context "with some values" $ do
      let val = reify ["foo", "bar", "baz", "qux"]
      it "returns a filled tree" $ do
        show (foldTree val) `shouldBe` "Node 2 (Node 0 Leaf \"bar\" Leaf) \"qux\" (Node 1 Leaf \"baz\" (Node 0 Leaf \"foo\" Leaf))"
  describe "xor" $ do
    context "with an odd number of Trues in a list" $ do
      let val = [False, True, False]
      it "returns True" $ do
        xor val `shouldBe` True
    context "with an even number of Trues in a list" $ do
      let val = [False, True, False, True]
      it "returns False" $ do
        xor val `shouldBe` False
  describe "map'" $ do
    context "some integers" $ do
      let
        val = [2,3,4] :: [Integer]
        f = (*2)
      it "acts like the prelude map" $ do
        map' f val `shouldBe` map f val
  describe "sieveSundaram" $ do
    context "with zero" $ do
      it "returns an empty list" $ do
        sieveSundaram 0 `shouldBe` []
    context "with non-zero positive" $ do
      it "returns the list of sieves" $ do
        sieveSundaram 10 `shouldBe` [3,5,7,11,13,17,19]
