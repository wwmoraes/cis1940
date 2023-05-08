{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant id" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Alternative law, left identity" #-}
{-# HLINT ignore "Alternative law, right identity" #-}

module AParserSpec (spec) where

import AParser
import Control.Applicative   (Alternative (empty, (<|>)))
import Data.Char             (isAlpha, isAlphaNum, isAscii, ord, toUpper)
import Provided.AParser
import SExpr                 (oneOrMore)
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck       ()

spec :: Spec
spec = do
  describe "Parser" $ do
    let
      aParser = satisfy isAlpha
      bParser = oneOrMore (satisfy isAlphaNum)
      cParser = oneOrMore (satisfy isAlpha)
      dParser = oneOrMore (satisfy isAscii)
      f1 = ord
      f2 = toUpper
      flatten (a, (b, c)) = (a, b, c)
    context "as a Functor" $ do
      prop "respects the Identity law" $ shouldBe
        <$> runParser (fmap id aParser)
        <*> runParser (id aParser)
      prop "respects the Composition law" $ shouldBe
        <$> runParser (fmap (f1 . f2) aParser)
        <*> runParser (fmap f1 $ fmap f2 aParser)
    context "as an Applicative" $ do
      prop "respects the Identity law" $ shouldBe
        <$> runParser (pure id <*> aParser)
        <*> runParser aParser
      prop "respects the Composition law" $ shouldBe
        <$> runParser (pure (,,) <*> dParser <*> cParser <*> bParser)
        <*> runParser (flatten <$> (pure (,) <*> dParser <*> (pure (,) <*> cParser <*> bParser)))
      prop "respects the Homomorphism law" $ \s -> shouldBe
        <$> runParser (pure (map f2) <*> pure s)
        <*> runParser (pure (map f2 s))
      prop "respects the Interchange law" $ \s -> shouldBe
        <$> runParser (pure (map f2) <*> pure (s :: String))
        <*> runParser (pure ($ s) <*> pure (map f2))
    context "as an Alternative" $ do
      prop "respects the Identity law" $ shouldBe
        <$> runParser (empty <|> bParser)
        <*> runParser (bParser <|> empty)
  describe "abParser" $ do
    -- let
    --   hasPrefix ('a':'b':_) = True
    --   hasPrefix _           = False
    -- prop "parses ab-prefixed strings" $ do
    --   \x -> hasPrefix x
    --     ==> runParser abParser x `shouldBe` Just (('a', 'b'), drop 2 x)
    context "with an empty string" $ do
      it "returns Nothing" $ do
        runParser abParser "" `shouldBe` Nothing
    context "with an ab-prefixed string" $ do
      let with = "abcdef"
      it "returns a tuple and rest" $ do
        let want = (('a', 'b'), "cdef")
        runParser abParser with `shouldBe` Just want
    context "with an non-ab-prefixed string" $ do
      let with = "aebcdf"
      it "returns Nothing" $ do
        runParser abParser with `shouldBe` Nothing
  describe "abParser_" $ do
    context "with an empty string" $ do
      it "returns Nothing" $ do
        runParser abParser_ "" `shouldBe` Nothing
    context "with an ab-prefixed string" $ do
      let with = "abcdef"
      it "returns unit and rest" $ do
        let want = ((), "cdef")
        runParser abParser_ with `shouldBe` Just want
    context "with an non-ab-prefixed string" $ do
      let with = "aebcdf"
      it "returns Nothing" $ do
        runParser abParser_ with `shouldBe` Nothing
  describe "intPair" $ do
    context "with valid input" $ do
      let
        input = "12 34"
        want = [12, 34] :: [Integer]
      it "returns the pair or integers" $ do
        runParser intPair input `shouldBe` Just (want, "")
  describe "intOrUppercase" $ do
    context "with valid input" $ do
      let
        checks = [
            ("342abcd", Just ((), "abcd")),
            ("XYZ", Just ((), "YZ")),
            ("foo", Nothing)
          ]
      it "returns the rest or Nothing" $ do
        foldMap (uncurry shouldBe . first (runParser intOrUppercase)) checks
