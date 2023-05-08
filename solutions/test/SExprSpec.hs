module SExprSpec (spec) where

import SExpr
import Provided.AParser
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck       ()

spec :: Spec
spec = do
  describe "parseSExpr" $ do
    context "with sample S-expressions" $ do
      let
        samples =
          [ "5"
          , "foo3"
          , "(bar (foo) 3 5 874)"
          , "(((lambda x (lambda y (plus x y))) 3) 5)"
          , "(   lots  of   (  spaces   in  )  this ( one ) )"
          ]
      it "parses completely with no remainder" $ do
        foldMap ((`shouldBe` Just "") . fmap snd . runParser parseSExpr) samples
    context "with a complete S-expression" $ do
      let
        sample = "( AB123 456 )"
        want = "Just (Comb [A (I \"AB123\"),A (N 456)],\"\")"
      it "is shown" $ do
        show (runParser parseSExpr sample) `shouldBe` want
