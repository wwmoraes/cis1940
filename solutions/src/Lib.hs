{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Lib
-- Description : Homework 1 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 1 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Lib
  ( -- * Exercise 1
    toDigits,
    toDigitsRev,

    -- * Exercise 2
    doubleEveryOther,
    doubleEveryOtherRev,

    -- * Exercise 3
    sumDigits,

    -- * Exercise 4
    validate,

    -- * Exercise 5
    Peg,
    Move,
    hanoi,

    -- * Exercise 6
    -- | TODO @hanoi4@, a function that solves the hanoi problem with 4 pegs
  )
where

-- * Exercise 1

-- | 'toDigits' returns a high-order-first list with each digit
--
-- >>> toDigits 1234
-- [1,2,3,4]
toDigits :: Integer -> [Integer]
toDigits n | n <= 0 = []
toDigits n = reverse $ toDigitsRev n

-- | 'toDigitsRev' returns a low-order-first list with each digit
--
-- >>> toDigitsRev 1234
-- [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n <= 0 = []
toDigitsRev n = (:) <$> flip mod 10 <*> toDigitsRev . flip div 10 $ n

-- * Exercise 2

-- | 'doubleEveryOther' doubles every even element. right-associative.
--
-- >>> doubleEveryOther [2,3,4,5]
-- [4,3,8,5]
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- | 'doubleEveryOtherRev' doubles every even element. left-associative.
--
-- >>> doubleEveryOtherRev [2,3,4,5]
-- [2,6,4,10]
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev []           = []
doubleEveryOtherRev [x]          = [x]
doubleEveryOtherRev (x : y : zs) = x : y * 2 : doubleEveryOtherRev zs

-- * Exercise 3

-- | 'sumDigits' sums all numbers after applying `toDigits` to each
sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits

-- * Exercise 4

-- | 'validate' checks if a credit card number is valid
validate :: Integer -> Bool
validate = (0 ==) . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- * Exercise 5

-- | 'Peg' represents a hanoi tower peg
type Peg = String

-- | 'Move' is a pair of source and destination pegs, representing a piece move
type Move = (Peg, Peg)

-- | 'hanoi' executes the hanoi game logic with n pieces on 3 pegs, and returns
-- the sequence of movements to solve it
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 src dst _ = [(src, dst)]
hanoi n src dst aux = prev ++ this ++ next
  where
    prev = hanoi (n - 1) src aux dst
    this = [(src, dst)]
    next = hanoi (n - 1) aux dst src

-- * Exercise 6

-- hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
-- hanoi4 = undefined

-- hanoi4 0 _ _ _ _ = []
-- hanoi4 1 src dst _ _ = [(src, dst)]
-- hanoi4 n src dst aux1 aux2 = undefined

{-
hanoi logic with 3 pegs, 2 pieces
  - push b . pop a [[2], [1], []]
  - push c . pop a [[], [1], [2]]
  - push c . pop b [[], [], [1,2]]

hanoi logic with 3 pegs, 3 pieces
  - push b . pop a [[2,3], [1], []]
  - push c . pop a [[3], [1], [2]]
  - push c . pop b [[3], [], [1,2]]

  - push b . pop a [[], [3], [1,2]]
  - push a . pop c [[1], [3], [2]]
  - push b . pop c [[1], [2,3], []]
  - pubh b . pop a [[], [1,2,3], []]

-}
