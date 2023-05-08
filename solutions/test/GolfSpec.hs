module GolfSpec (spec) where

import Golf (histogram, localMaxima, skips)
import Test.Hspec

spec :: Spec
spec = do
  describe "skips" $ do
    context "with an empty list" $ do
      it "should return an empty list" $ do
        skips [] `shouldBe` ([] :: [[Char]])
    context "with a single element list" $ do
      it "should return an identity sub-list" $ do
        skips [1] `shouldBe` ([[1]] :: [[Int]])
    context "with a string" $ do
      it "should return the skip substrings" $ do
        skips "ABCD" `shouldBe` (["ABCD", "BD", "C", "D"] :: [String])
    describe "localMaxima" $ do
      context "with an ordered sequence" $ do
        it "should return an empty list" $ do
          localMaxima [1, 2, 3, 4, 5] `shouldBe` []
      context "with a semi-ordered list and a maxima pivot" $ do
        it "should return the maxima value" $ do
          localMaxima [2, 3, 4, 1, 5] `shouldBe` [4]
      context "with a unordered list with multiple maxima values" $ do
        it "should return the maxima values" $ do
          localMaxima [2, 9, 5, 6, 1] `shouldBe` [9, 6]
    describe "histogram" $ do
      context "with an empty list" $ do
        it "should return an empty histogram" $ do
          histogram ([] :: [Integer])
            `shouldBe` unlines
              [ "==========",
                "0123456789"
              ]
      context "with valid values" $ do
        it "should return a histogram" $ do
          histogram ([1, 1, 1, 5] :: [Integer])
            `shouldBe` unlines
              [ " *        ",
                " *        ",
                " *   *    ",
                "==========",
                "0123456789"
              ]
        it "should return a histogram" $ do
          histogram ([1, 4, 5, 4, 6, 6, 3, 4, 2, 4, 9] :: [Integer])
            `shouldBe` unlines
              [ "    *     ",
                "    *     ",
                "    * *   ",
                " ******  *",
                "==========",
                "0123456789"
              ]
