module ExprTSpec (spec) where

import Provided.ExprT
import Test.Hspec

spec :: Spec
spec = do
  describe "ExprT" $ do
    context "with an arbitrary expression" $ do
      let
        expr1 = Add (Lit 3) $ Mul (Lit 2) (Lit 3)
        expr2 = Add (Lit 3) $ Mul (Lit 2) (Lit 3)
      it "can be shown" $ do
        show expr1 `shouldBe` "Add (Lit 3) (Mul (Lit 2) (Lit 3))"
      it "is equal" $ do
        expr1 == expr2 `shouldBe` True
