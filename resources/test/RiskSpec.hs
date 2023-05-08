module RiskSpec (spec) where

import Control.Monad.Random
import Data.Functor          ((<&>))
import Provided.Risk
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "die" $ do
    let
      valid :: DieValue -> Bool
      valid = (&&) <$> (>=1) <*> (<=6)
    prop "6-sided roll" $ do
      result <- evalRandIO die
      result `shouldSatisfy` valid
    context "with a one-sided dice" $ do
      let
        dice :: Rand StdGen DieValue
        dice = getRandomR (6, 6)
        want = DV 6
        dv5 = DV 5
      it "equals to 6" $ do
        result <- evalRandIO dice
        result == want `shouldBe` True
        result /= dv5 `shouldBe` True
      it "is higher than 5" $ do
        (evalRandIO dice <&> compare dv5) `shouldReturn` LT
      it "is shown" $ do
        (evalRandIO dice <&> show) `shouldReturn` show want
  describe "Battlefield" $ do
    context "with a sample" $ do
      let
        a1 = 3
        a2 = 2
        bf = Battlefield a1 a2
      it "has 3 attackers" $ do
        attackers bf `shouldBe` a1
      it "has 2 defenders" $ do
        defenders bf `shouldBe` a2
