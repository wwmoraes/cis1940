module LogSpec (spec) where

import Provided.Log
import Test.Hspec

import Paths_cis1940_resources (getDataFileName)

spec :: Spec
spec = do
  let
    valInfo = LogMessage Info 100 "foo"
    valWarning = LogMessage Warning 150 "bar"
    valError = LogMessage (Error 50) 200 "baz"
  describe "MessageType" $ do
    context "with an Info" $ do
      it "is equal to Info" $ do
        Info == Info `shouldBe` True
      it "is not equal to Warning" $ do
        Info == Warning `shouldBe` False
  describe "LogMessage" $ do
    context "with a valid info log message" $ do
      it "should show" $ do
        show valInfo `shouldBe` "LogMessage Info 100 \"foo\""
      it "is equal to LogMessage Info" $ do
        valInfo == valInfo `shouldBe` True
      it "is not equal to LogMessage Warning" $ do
        valInfo == valWarning `shouldBe` False
  describe "MessageTree" $ do
    context "with a valid tree" $ do
      let
        valInfoMT = Node Leaf valInfo Leaf
        valErrorMT = Node Leaf valError Leaf
      it "should show" $ do
        show valInfoMT `shouldBe` "Node Leaf (LogMessage Info 100 \"foo\") Leaf"
      it "should be equal to the same tree" $ do
        valInfoMT == valInfoMT `shouldBe` True
      it "should not be equal to a different tree" $ do
        valInfoMT == valErrorMT `shouldBe` False
  describe "testParse" $ do
    context "with a const parser" $ do
      let
        constWarning :: String -> [LogMessage]
        constWarning = const [valWarning]
      it "returns the const log" $ do
        testFile <- getDataFileName "empty.log"
        testParse constWarning 1 testFile `shouldReturn` [valWarning]
  describe "testWhatWentWrong" $ do
    context "with a const parser and filter" $ do
      let
        constError :: String -> [LogMessage]
        constError = const [valError]
        passthrough :: [LogMessage] -> [String]
        passthrough = map show
      it "returns the const log" $ do
        testFile <- getDataFileName "empty.log"
        testWhatWentWrong constError passthrough testFile `shouldReturn` [show valError]
