module RiskSpec (spec) where

import Risk
import Provided.Risk
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck       ()
import Control.Monad.Random

showBattlefield :: Battlefield -> IO ()
showBattlefield bf = do
  putStrLn $ "attackers: " ++ show (attackers bf)
  putStrLn $ "defenders: " ++ show (defenders bf)

-- showBattlefield =<< (evalRandIO $ battle $ Battlefield { attackers = 3, defenders = 3 })
-- showBattlefield =<< (evalRandIO $ invade $ Battlefield { attackers = 3, defenders = 3 })
-- evalRandIO $ successProb $ Battlefield { attackers = 3, defenders = 3 }

spec :: Spec
spec = do
  let
    sample = Battlefield { attackers = 3, defenders = 3 }
    defenderWin = Battlefield { attackers = 0, defenders = 1 }
    attackerWin = Battlefield { attackers = 1, defenders = 0 }
    equal abf bbf | attackers abf /= attackers bbf = False
    equal abf bbf | defenders abf /= defenders bbf = False
    equal _ _ = True
  describe "battle" $ do
    context "with sample battlefield" $ do
      it "runs successfully" $ do
        result <- evalRandIO $ battle sample
        attackers result <= attackers sample `shouldBe` True
        defenders result <= defenders sample `shouldBe` True
    context "with an attacker win scenario" $ do
      it "does nothing" $ do
        result <- evalRandIO $ battle attackerWin
        equal result attackerWin `shouldBe` True
    context "with a defender win scenario" $ do
      it "does nothing" $ do
        result <- evalRandIO $ battle defenderWin
        equal result defenderWin `shouldBe` True
  describe "invade" $ do
    let
      valid :: Battlefield -> Bool
      valid = (||) <$> (== 1) . attackers <*> (== 0) . defenders
    it "runs successfully" $ do
      result <- evalRandIO $ invade sample
      valid result `shouldBe` True
  describe "successProb" $ do
    let
      valid :: Double -> Bool
      valid = (&&) <$> (<= 1.0) <*> (>= 0.0)
    it "runs successfully" $ do
      result <- evalRandIO $ successProb sample
      valid result `shouldBe` True
