module StackVMSpec (spec) where

import Provided.StackVM
import Test.Hspec

spec :: Spec
spec = do
  describe "stackVM" $ do
    let
      illProgram op = [PushB True, PushI 3, op] :: Program
      intProgram op = [PushI 2, PushI 3, op, PushI 4, op] :: Program
      boolProgram1 op = [PushB False, PushB True, op, PushB False, op] :: Program
      boolProgram2 op = [PushB True, PushB False, op, PushB True, op] :: Program
      rightInt :: Integer -> String
      rightInt x = "Right (IVal " ++ show x ++ ")"
      rightBool :: Bool -> String
      rightBool x = "Right (BVal " ++ show x ++ ")"
      underflowErr op = "Left \"Stack underflow with '" ++ show op ++ "' opcode.\""
      illStackErr op = "Left \"Encountered '" ++ show op ++ "' opcode with ill-typed stack.\""
    context "with an empty program" $ do
      let program = [] :: Program
      it "returns Right Void" $ do
        show (stackVM program) `shouldBe` "Right Void"
    context "with constant values" $ do
      it "returns Right value for an Integer" $ do
        let program = [PushI 1] :: Program
        show (stackVM program) `shouldBe` rightInt 1
      it "returns Right value for a Bool" $ do
        let program = [PushB True] :: Program
        show (stackVM program) `shouldBe` rightBool True
    context "with an Add operation" $ do
      let
        op = Add
        result = rightInt 9
      it "returns error without arguments" $ do
        show (stackVM [op]) `shouldBe` underflowErr op
      it "returns error with ill-typed arguments" $ do
        show (stackVM $ illProgram op) `shouldBe` illStackErr op
      it "returns value with well-typed arguments" $ do
        show (stackVM $ intProgram op) `shouldBe` result
    context "with an Mul operation" $ do
      let
        op = Mul
        result = rightInt 24
      it "returns error without arguments" $ do
        show (stackVM [op]) `shouldBe` underflowErr op
      it "returns error with ill-typed arguments" $ do
        show (stackVM $ illProgram op) `shouldBe` illStackErr op
      it "returns value with well-typed arguments" $ do
        show (stackVM $ intProgram op) `shouldBe` result
    context "with an And operation" $ do
      let
        op = And
        result = rightBool False
      it "returns error without arguments" $ do
        show (stackVM [op]) `shouldBe` underflowErr op
      it "returns error with ill-typed arguments" $ do
        show (stackVM $ illProgram op) `shouldBe` illStackErr op
      it "returns value with well-typed arguments" $ do
        show (stackVM $ boolProgram1 op) `shouldBe` result
      it "returns value with well-typed arguments (inverted values)" $ do
        show (stackVM $ boolProgram2 op) `shouldBe` result
    context "with an Or operation" $ do
      let
        op = Or
        result = rightBool True
      it "returns error without arguments" $ do
        show (stackVM [op]) `shouldBe` underflowErr op
      it "returns error with ill-typed arguments" $ do
        show (stackVM $ illProgram op) `shouldBe` illStackErr op
      it "returns value with well-typed arguments" $ do
        show (stackVM $ boolProgram1 op) `shouldBe` result
      it "returns value with well-typed arguments (inverted values)" $ do
        show (stackVM $ boolProgram2 op) `shouldBe` result
