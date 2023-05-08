module CalcSpec (spec) where

import Calc
import Provided.ExprT
import Provided.Parser
import Test.Hspec

spec :: Spec
spec = do
  let
    testExpr = "(3 * -4) + 5"
    validExp :: Expr a => Maybe a
    validExp = parseExp lit add mul testExpr
  describe "eval" $ do
    context "with a valid expression" $ do
      it "should return the result" $ do
        eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) `shouldBe` (20 :: Integer)
  describe "evalStr" $ do
    context "with a valid expression" $ do
      it "should return the result" $ do
        evalStr "(2+3)*4" `shouldBe` (Just 20 :: Maybe Integer)
    context "with an invalid expression" $ do
      it "should return nothing" $ do
        evalStr "(2+3)*" `shouldBe` (Nothing :: Maybe Integer)
  describe "ExprT" $ do
    context "with an inline expression" $ do
      it "returns the value" $ do
        eval (mul (add (lit 2) (lit 3)) (lit 4)) `shouldBe` 20
  describe "Expr Integer" $ do
    context "with sample expression" $ do
      let
        reify :: Maybe Integer -> Maybe Integer
        reify = id
        source = reify validExp
        want = reify $ Just (-7)
      it "returns -7" $ do
        source `shouldBe` want
        validExp == want `shouldBe` True
  describe "Expr Bool" $ do
    context "with sample expression" $ do
      let
        reify :: Maybe Bool -> Maybe Bool
        reify = id
        source = reify validExp
        want = reify $ Just True
      it "returns True" $ do
        source `shouldBe` want
        validExp == want `shouldBe` True
      it "can be shown" $ do
        show source `shouldBe` show want
  describe "Expr MinMax" $ do
    context "with sample expression" $ do
      let
        reify :: Maybe MinMax -> Maybe MinMax
        reify = id
        source = reify validExp
        want = reify $ Just (MinMax 5)
      it "returns 5" $ do
        source `shouldBe` want
        validExp == want `shouldBe` True
      it "can be shown" $ do
        show source `shouldBe` show want
  describe "Expr Mod7" $ do
    context "with sample expression" $ do
      let
        reify :: Maybe Mod7 -> Maybe Mod7
        reify = id
        source = reify validExp
        want = reify $ Just (Mod7 0)
      it "returns 0" $ do
        source `shouldBe` want
        validExp == want `shouldBe` True
      it "can be shown" $ do
        show source `shouldBe` show want
  describe "Expr VM.Program" $ do
    context "with sample expression" $ do
      it "executes successfully" $ do
        show (executeVM testExpr) `shouldBe` "Right (IVal (-7))"
    context "with an invalid expression" $ do
      it "returns nothing" $ do
        show (executeVM "1+") `shouldBe` "Right Void"
  describe "VarExprT" $ do
    context "with a valid expression" $ do
      let
        testVExprT :: VarExprT
        testVExprT = add (mul (lit 3) (var "x")) (lit 5)
      it "is shown" $ do
        show testVExprT `shouldBe` "VAdd (VMul (VLit 3) (VVar \"x\")) (VLit 5)"
      it "is equal" $ do
        testVExprT == add (mul (lit 3) (var "x")) (lit 5) `shouldBe` True
  describe "withVars" $ do
    context "with a valid expression" $ do
      let varExp = add (mul (lit 3) (var "x")) (lit 5)
      it "executes successfully" $ do
        withVars [("x", -4)] varExp `shouldBe` Just (-7)
