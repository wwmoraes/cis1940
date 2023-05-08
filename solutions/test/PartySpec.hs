module PartySpec (spec) where

import Party
import Provided.Employee

import Data.Tree (Tree (Node))

import Control.Exception (evaluate)
import Test.Hspec

spec :: Spec
spec = do
  let
    emp2 = Emp "Foo" 2
    emp3 = Emp "Bar" 3
    gl2 = GL [emp2] (empFun emp2)
    gl3 = GL [emp3] (empFun emp3)
    t2 = Node emp2 []

  describe "moreFun" $ do
    context "with an ascending pair" $ do
      let pair = (gl2, gl3)
      it "returns the second element" $ do
        uncurry moreFun pair `shouldBe` snd pair
    context "with a descending pair" $ do
      let pair = (gl3, gl2)
      it "returns the second element" $ do
        uncurry moreFun pair `shouldBe` fst pair

  describe "treeFold" $ do
    let
      formatEmp :: Employee -> String
      formatEmp e = empName e ++ " (" ++ (show $ empFun e) ++ ")\n"
      want = unlines
        [ "Stan (9)"
        , "Bob (2)"
        , "Joe (5)"
        , "John (1)"
        , "Sue (5)"
        , "Fred (3)"
        , "Sarah (17)"
        , "Sam (4)"
        ]
    context "with testCompany" $ do
      it "returns the formatted list of employees" $ do
        treeFold formatEmp testCompany `shouldBe` want

  describe "maxFun" $ do
    context "with a single employee" $ do
      it "returns the single employee" $ do
        maxFun t2 `shouldBe` gl2
    context "with testCompany" $ do
      let want = GL
            [ Emp "Bob" 2
            , Emp "John" 1
            , Emp "Sue" 5
            , Emp "Sarah" 17
            ] 25
      it "returns the maximum fun guest list" $ do
        maxFun testCompany `shouldBe` want
    context "with testCompany2" $ do
      let want = GL
            [ Emp "Bob" 3
            , Emp "John" 1
            , Emp "Sue" 5
            , Emp "Sarah" 17
            ] 26
      it "returns the maximum fun guest list" $ do
        maxFun testCompany2 `shouldBe` want
  describe "parseHierarchy" $ do
    context "empty string" $ do
      let errReadNoParse = "Prelude.read: no parse"
      it "throws a read exception" $ do
        evaluate (parseHierarchy "") `shouldThrow` errorCall errReadNoParse
    context "valid input" $ do
      let
        nodeStr = "Node {rootLabel = Emp {empName = \"Foo\", empFun = 5}, subForest = []}"
        emp = Emp "Foo" 5
        node = Node emp []
      it "returns the parsed tree" $ do
        parseHierarchy nodeStr `shouldBe` node

  describe "glFun" $ do
    context "with empty guest list" $ do
      it "returns zero" $ do
        glFun mempty `shouldBe` 0
    context "with non-empty guest list" $ do
      it "returns total fun" $ do
        glFun gl2 `shouldBe` empFun emp2

  describe "glGuests" $ do
    context "with empty guest list" $ do
      it "returns empty string list" $ do
        glGuests mempty `shouldBe` []
    context "with non-empty guest list" $ do
      it "returns the employees' names" $ do
        glGuests gl2 `shouldBe` [empName emp2]
