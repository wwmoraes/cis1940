module ScrabbleSpec (spec) where

import Data.Foldable (fold)
import Scrabble
import Test.Hspec

spec :: Spec
spec = do
  let checks = [
        ('a', 1),
        ('b', 3),
        ('c', 3),
        ('d', 2),
        ('e', 1),
        ('f', 4),
        ('g', 2),
        ('h', 4),
        ('i', 1),
        ('j', 8),
        ('k', 5),
        ('l', 1),
        ('m', 3),
        ('n', 1),
        ('o', 1),
        ('p', 3),
        ('q', 10),
        ('r', 1),
        ('s', 1),
        ('t', 1),
        ('u', 1),
        ('v', 4),
        ('w', 4),
        ('x', 8),
        ('y', 4),
        ('z', 10),
        (' ', 0)
        ] :: [(Char, Int)]
  describe "score" $ do
    context "with letters and space" $ do
      it "returns the score" $ do
        fold [score val `shouldBe` Score want | (val, want) <- checks]
      it "is equal" $ do
        fold [score val == Score want `shouldBe` True | (val, want) <- checks]
      it "is shown" $ do
        fold [show (score val) `shouldBe` "Score " ++ show want | (val, want) <- checks]
