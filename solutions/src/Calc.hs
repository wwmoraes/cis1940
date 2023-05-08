{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Calc
-- Description : Homework 5 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 5 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Calc
  ( -- * Exercise 1
    eval,

    -- * Exercise 2
    evalStr,

    -- * Exercise 3
    Expr (..),

    -- * Exercise 4
    MinMax (..),
    Mod7 (..),

    -- * Exercise 5
    compile,

    -- * Exercise 6
    HasVars (..),
    VarExprT(..),
    withVars,

    -- * GHCi utilities
    executeVM,
  )
where

import qualified Data.Map         as M
import           Data.Maybe       (fromMaybe)
import           GHC.Base         (Applicative (liftA2))
import           Provided.ExprT
import           Provided.Parser  (parseExp)
import qualified Provided.StackVM as VM

-- * Exercise 1

eval :: ExprT -> Integer
eval (Lit x)   = x
eval (Add a b) = (+) (eval a) (eval b)
eval (Mul a b) = (*) (eval a) (eval b)

-- * Exercise 2

evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp Lit Add Mul

-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- reify :: ExprT -> ExprT
-- reify = id

-- Exercise 4

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax a) (MinMax b) = MinMax $ max a b
  mul (MinMax a) (MinMax b) = MinMax $ min a b

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . flip mod 7
  add (Mod7 a) (Mod7 b) = Mod7 . flip mod 7 $ (+) a b
  mul (Mod7 a) (Mod7 b) = Mod7 . flip mod 7 $ (*) a b

-- Exercise 5

instance Expr VM.Program where
  lit = pure . VM.PushI
  add a b = a ++ b ++ pure VM.Add
  mul a b = a ++ b ++ pure VM.Mul

compile :: String -> Maybe VM.Program
compile = parseExp lit add mul

executeVM :: String -> Either String VM.StackVal
executeVM = VM.stackVM . fromMaybe [] . compile

-- Exercise 6

class HasVars a where
  var :: String -> a

data VarExprT
  = VLit Integer
  | VAdd VarExprT VarExprT
  | VMul VarExprT VarExprT
  | VVar String
  deriving (Show, Eq)

instance Expr VarExprT where
  lit = VLit
  add = VAdd
  mul = VMul

instance HasVars VarExprT where
  var = VVar

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit a _ = Just a
  add a b = liftA2 (+) <$> a <*> b
  mul a b = liftA2 (*) <$> a <*> b

withVars :: [(String, Integer)] -> (M.Map String Integer -> Maybe Integer) -> Maybe Integer
withVars vs ex = ex $ M.fromList vs
