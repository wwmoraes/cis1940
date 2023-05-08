module EmployeeSpec (spec) where

import Data.List (intercalate)

import Provided.Employee
import Test.Hspec

type TestEmp = (Name, Fun)
newtype TestNode = TestNode (TestEmp, [TestNode])

spec :: Spec
spec = do
  describe "Employee" $ do
    let
      empStr :: TestEmp -> String
      empStr (a, b) = concat
        [ "Emp {empName = "
        , show a
        , ", empFun = "
        , show b
        , "}"
        ]
      nodeStr :: TestNode -> String
      nodeStr (TestNode (emp, xs))
        = concat
          [ "Node {rootLabel = "
          , empStr emp
          , ", subForest = ["
          , intercalate "," $ map nodeStr xs
          , "]}"
          ]
    context "with single value" $ do
      let
        name = "foo"
        fun = 10
        emp = Emp name fun
      it "is shown" $ do
        show emp `shouldBe`
          "Emp {empName = "++ show name ++", empFun = "++ show fun ++"}"
      it "is equal" $ do
        emp == Emp name fun `shouldBe` True
      it "returns name" $ do
        empName emp `shouldBe` name
      it "returns fun score" $ do
        empFun emp `shouldBe` fun
    context "with testCompany" $ do
      let testCompanyStr = nodeStr $
            TestNode (("Stan", 9),
            [ TestNode (("Bob", 2),
              [ TestNode (("Joe", 5),
                [ TestNode (("John", 1), [])
                , TestNode (("Sue", 5), [])
                ])
              , TestNode (("Fred", 3), [])
              ])
            , TestNode (("Sarah", 17),
              [ TestNode (("Sam", 4), [])
              ])
            ])
      it "is shown" $ do
        show testCompany `shouldBe` testCompanyStr
    context "with testCompany2" $ do
      let testCompany2Str = nodeStr $
            TestNode (("Stan", 9),
            [ TestNode (("Bob", 3),
              [ TestNode (("Joe", 5),
                [ TestNode (("John", 1), [])
                , TestNode (("Sue", 5), [])
                ])
              , TestNode (("Fred", 3), [])
              ])
            , TestNode (("Sarah", 17),
              [ TestNode (("Sam", 4), [])
              ])
            ])
      it "is shown" $ do
        show testCompany2 `shouldBe` testCompany2Str
