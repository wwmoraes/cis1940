module JoinListSpec (spec) where

import Provided.JoinList
import Test.Hspec

spec :: Spec
spec = do
  describe "JoinList" $ do
    context "with an arbitrary value" $ do
      let
        node = Single (Just 1) "foo"
        jl1 = Append (Just 1) node Empty :: JoinList (Maybe Int) String
        jl2 = Append (Just 1) node Empty :: JoinList (Maybe Int) String
      it "can be shown" $ do
        show jl1 `shouldBe` "Append (Just 1) (Single (Just 1) \"foo\") Empty"
      it "is equal" $ do
        jl1 == jl2 `shouldBe` True
  describe "jlToList" $ do
    let
      reify :: JoinList (Maybe Int) String -> JoinList (Maybe Int) String
      reify = id
      valEmpty = reify Empty
      valSingle1 = reify $ Single (Just 1) "foo"
      valSingle2 = reify $ Single (Just 1) "bar"
      valAppend = reify $ Append (Just 2) valSingle1 valSingle2
    context "with an Empty JoinList" $ do
      it "returns an empty list" $ do
        jlToList valEmpty `shouldBe` []
    context "with a Single JoinList" $ do
      it "returns a single-value list" $ do
        jlToList valSingle1 `shouldBe` ["foo"]
    context "with an Append JoinList" $ do
      it "returns a single-value list" $ do
        jlToList valAppend `shouldBe` ["foo", "bar"]
  describe "!!?" $ do
    let reify :: [String] -> [String]
        reify = id
        valEmpty = reify []
        valSome = reify ["foo", "bar", "baz"]
    context "with an empty list" $ do
      it "returns Nothing" $ do
        valEmpty !!? 0 `shouldBe` Nothing
    context "with a non-empty list" $ do
      it "returns Nothing for negative index" $ do
        valSome !!? (-1) `shouldBe` Nothing
      it "returns value for valid index" $ do
        valSome !!? 1 `shouldBe` Just "bar"
