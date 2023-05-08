module JoinListSpec (spec) where

import Data.Foldable     (Foldable (fold))
import JoinList
import Provided.Buffer
import Provided.JoinList
import Provided.Sized
import Scrabble
import Test.Hspec

spec :: Spec
spec = do
  let
    -- emptyList = [] :: [()]
    -- filledList = [1, 2, 3, 4]
    reify :: JoinList (Score, Size) String -> JoinList (Score, Size) String
    reify = id
    emptyVal = reify $ Empty
    aVal = reify $ Single (Score 6, Size 1) "foo"
    bVal = reify $ Single (Score 5, Size 1) "bar"
    abVal = reify $ Append (Score 11, Size 2) aVal bVal
    yeahVal = reify $ Append
        (Score 10, Size 4)
        (Single (Score 4, Size 1) "y")
        ( Append
          (Score 6, Size 3)
          (Single (Score 1, Size 1) "e")
          ( Append
            (Score 5, Size 2)
            (Single (Score 1, Size 1) "a")
            (Single (Score 4, Size 1) "h")
          )
        )
  describe "(+++)" $ do
    context "with empty and a single value" $ do
      it "returns the single value" $ do
        emptyVal +++ aVal `shouldBe` aVal
        aVal +++ emptyVal `shouldBe` aVal
    context "with two single values" $ do
      it "returns an append value" $ do
        aVal +++ bVal `shouldBe` abVal
  describe "tag" $ do
    context "with an empty value" $ do
      it "returns an empty metadata" $ do
        tag emptyVal `shouldBe` mempty
  describe "indexJ" $ do
    context "with an empty value" $ do
      it "returns Nothing" $ do
        indexJ 0 emptyVal `shouldBe` Nothing
    context "with some value" $ do
      it "returns Nothing for negative indexes" $ do
        indexJ (-1) abVal `shouldBe` Nothing
      it "returns the value at the index" $ do
        indexJ 0 abVal `shouldBe` Just "foo"
      it "returns Nothing for out-of-bounds indexes" $ do
        indexJ 2 abVal `shouldBe` Nothing
  describe "dropJ" $ do
    context "with an empty value" $ do
      it "returns itself" $ do
        dropJ 1 emptyVal `shouldBe` emptyVal
    context "with some value" $ do
      it "returns itself for negative indexes" $ do
        dropJ (-1) abVal `shouldBe` abVal
      it "returns the resulting list" $ do
        dropJ 1 abVal `shouldBe` bVal
      it "returns an empty value when dropping all items" $ do
        dropJ 2 abVal `shouldBe` emptyVal
  describe "takeJ" $ do
    context "with an empty value" $ do
      it "returns itself" $ do
        takeJ 1 emptyVal `shouldBe` emptyVal
    context "with some value" $ do
      it "returns an empty value when taking no items" $ do
        takeJ (-1) abVal `shouldBe` emptyVal
      it "returns the resulting list" $ do
        takeJ 1 abVal `shouldBe` aVal
      it "returns itself when taking all items" $ do
        takeJ 2 abVal `shouldBe` abVal
  describe "scoreLine" $ do
    context "with an empty string" $ do
      it "returns the proper score" $ do
        scoreLine "qux" `shouldBe` Single (Score 19) "qux"
  describe "foldable join list" $ do
    context "with a non-empty value" $ do
      it "returns the folded result" $ do
        foldMap id yeahVal `shouldBe` "yeah"
    context "with an empty value" $ do
      it "returns an empty string" $ do
        foldMap id emptyVal `shouldBe` ""
  describe "Buffered" $ do
    context "with a non-empty value" $ do
      it "returns the concatenated string" $ do
        toString yeahVal `shouldBe` "y\ne\na\nh\n"
      it "returns the line at index" $ do
        line 1 yeahVal `shouldBe` Just "e"
      it "replaces a line" $ do
        fold (replaceLine 3 "." yeahVal) `shouldBe` "yea."
      it "returns the number of lines" $ do
        numLines yeahVal `shouldBe` 4
      it "returns the value" $ do
        value yeahVal `shouldBe` 10
    context "with an empty value" $ do
      it "returns an empty string" $ do
        toString emptyVal `shouldBe` ""
      it "returns the number of lines" $ do
        numLines emptyVal `shouldBe` 0
      it "returns the value" $ do
        value emptyVal `shouldBe` 0
    context "with a source string" $ do
      let source = "y\ne\na\nh\n"
      it "returns the parsed join list" $ do
        fromString source `shouldBe` yeahVal
        -- fromString source `shouldBe` Single (Score 10, Size 1) "yeah"
