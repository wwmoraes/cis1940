module ParserSpec (spec) where

import Provided.Parser (parseExp)
import Test.Hspec

spec :: Spec
spec = do
  describe "parseExp" $ do
    context "with a minified, well-formed expression" $ do
      it "returns the parsed result" $ do
        parseExp id (+) (*) "2+3*4" `shouldBe` Just 14
    context "with a spaced, well-formed positive expression" $ do
      it "returns the parsed result" $ do
        parseExp id (+) (*) "2 + 3 * 4" `shouldBe` Just 14
    context "with a spaced, well-formed negative expression" $ do
      it "returns the parsed result" $ do
        parseExp id (+) (*) "2 + 3 * -4" `shouldBe` Just (-10)
    context "with a spaced, well-formed parenthesized expression" $ do
      it "returns the parsed result" $ do
        parseExp id (+) (*) "(2 + 3) * 4" `shouldBe` Just 20
    context "with a spaced, ill-formed parenthesized expression" $ do
      it "returns Nothing" $ do
        parseExp id (+) (*) "(2 + 3 * 4" `shouldBe` Nothing
    context "with ill-formed expression" $ do
      it "returns Nothing" $ do
        parseExp id (+) (*) "2+3*c" `shouldBe` Nothing
    context "with incomplete expression" $ do
      it "returns Nothing" $ do
        parseExp id (+) (*) "2+3*" `shouldBe` Nothing
    context "with an unsupported operation" $ do
      it "returns Nothing" $ do
        parseExp id (+) (*) "2 + 3 / 4" `shouldBe` Nothing
