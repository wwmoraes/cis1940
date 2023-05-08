{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Scrabble
-- Description : Homework 7 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 7 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Scrabble
  ( -- * Exercise 3
    score,
    scoreString,

    -- * 'JoinList' integration
    getScore,
    Score(..),
  )
where

newtype Score = Score Int
  deriving (Eq, Show)

instance Semigroup Score where
  (<>) (Score a) (Score b) = Score (a + b)

instance Monoid Score where
  mempty = Score 0

score :: Char -> Score
score 'a' = Score 1
score 'b' = Score 3
score 'c' = Score 3
score 'd' = Score 2
score 'e' = Score 1
score 'f' = Score 4
score 'g' = Score 2
score 'h' = Score 4
score 'i' = Score 1
score 'j' = Score 8
score 'k' = Score 5
score 'l' = Score 1
score 'm' = Score 3
score 'n' = Score 1
score 'o' = Score 1
score 'p' = Score 3
score 'q' = Score 10
score 'r' = Score 1
score 's' = Score 1
score 't' = Score 1
score 'u' = Score 1
score 'v' = Score 4
score 'w' = Score 4
score 'x' = Score 8
score 'y' = Score 4
score 'z' = Score 10
score _   = mempty

scoreString :: String -> Score
scoreString = foldr ((<>) . score) mempty

getScore :: Score -> Int
getScore (Score x) = x
