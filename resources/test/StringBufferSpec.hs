module StringBufferSpec (spec) where

import Provided.Buffer
import Provided.StringBuffer ()
import Test.Hspec

spec :: Spec
spec = do
  describe "Buffer String" $ do
    context "with an empty value" $ do
      let val = ""
      it "equals to an empty string" $ do
        toString val `shouldBe` val
      it "equals itself when buffered" $ do
        fromString val `shouldBe` val
      it "returns nothing when line-indexed" $ do
        line 0 val `shouldBe` Nothing
      it "returns nothing when line-indexed with a negative index" $ do
        line (-1) val `shouldBe` Nothing
      it "returns itself without replacing anything" $ do
        replaceLine 0 "baz" val `shouldBe` val
      it "returns zero lines count" $ do
        numLines val `shouldBe` 0
      it "returns zero value" $ do
        value val `shouldBe` 0
    context "with some value" $ do
      let val = "foo\nbar\n"
      it "equals to the plain string" $ do
        toString val `shouldBe` val
      it "equals itself when buffered" $ do
        fromString val `shouldBe` val
      it "returns some value when line-indexed" $ do
        line 1 val `shouldBe` Just "bar"
      it "returns nothing when line-indexed with a negative index" $ do
        line (-1) val `shouldBe` Nothing
      it "returns a replaced value" $ do
        replaceLine 1 "baz" val `shouldBe` "foo\nbaz\n"
      it "returns lines count" $ do
        numLines val `shouldBe` 2
      it "returns some value" $ do
        value val `shouldBe` 2
