{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Fibonacci
-- Description : Homework 6 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 6 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Fibonacci
  ( -- * Exercise 1
    fib,
    fibs1,

    -- * Exercise 2
    fibs2,

    -- * Exercise 3
    streamToList,

    -- * Exercise 4
    streamRepeat,
    streamMap,
    streamFromSeed,

    -- * Exercise 5
    nats,
    ruler,

    -- * Exercise 6
    -- | TODO implement fibs3

    -- * Exercise 7
    -- | TODO implement fibs4
  )
where

import Data.List (uncons)
import GHC.Float (float2Int)

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- fibNext :: [Integer] -> [Integer]
-- fibNext [] = [0]
-- fibNext [0] = [1, 0]
-- fibNext (x : y : zs) = (x + y) : x : y : zs
-- fibNext [x] = [x] -- should only happen on tinkered inputs

fibs2 :: [Integer]
fibs2 = scanl (+) 0 (1 : fibs2)

-- Exercise 3

data Stream a = a :< Stream a

instance Show a => Show (Stream a) where
  -- show :: Show a => Stream a -> String
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (x :< xs) = x : streamToList xs

-- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat = (:<) <$> id <*> streamRepeat

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (x :< xs) = f x :< streamMap f xs

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f = (:<) <$> f <*> streamFromSeed f . f

-- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (x :< xs) ys = x :< interleaveStreams ys xs

ruler :: Stream Integer
ruler = streamMap rulerValue $ interleaveStreams (streamRepeat 0) (streamFromSeed (+ 2) 0)

rulerValue :: Integer -> Integer
rulerValue n | n <= 0 = 0
rulerValue n = takeFirst (rulerOf n) $ reverse [0 .. (rulerUpper n)]

rulerOf :: (Integral a, Integral b) => a -> b -> Bool
rulerOf n = (== 0) . mod n . (2 ^)

takeFirst :: Integral a => (a -> Bool) -> [a] -> a
takeFirst f = maybe 0 fst . uncons . take 1 . dropWhile (not . f)

rulerUpper :: Integer -> Integer
rulerUpper = fromIntegral . float2Int . logBase 2 . fromIntegral

-- Exercise 6

-- asStream :: [a] -> Stream a
-- asStream []       = error "empty lists cannot be converted to streams"
-- asStream [x]      = streamRepeat x
-- asStream (x : xs) = x :< asStream xs

-- fibs2a :: Stream Integer
-- fibs2a = asStream fibs2

-- fibs3 :: Stream Integer
-- fibs3 = undefined

-- Exercise 7

-- fibs4 :: Stream Integer
-- fibs4 = undefined
