{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : Golf
-- Description : Homework 3 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 3 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module Golf
  ( -- * Exercise 1
    skips,

    -- * Exercise 2
    localMaxima,

    -- * Exercise 3
    histogram,
  )
where

import Data.Bits  (Bits (bit, testBit, zeroBits, (.|.)))
import Data.List  (group, sort)
import Data.Maybe (mapMaybe)

-- * Exercise 1

skips :: [a] -> [[a]]
skips []  = []
skips [x] = [[x]]
skips xs  = foldr ((:) . skip xs) [] $ indexes xs

-- indexes returns the zero-based indexes of a list
indexes :: [a] -> [Int]
indexes = zipWith const [0 ..]

-- skip filters out and returns the element after every nth one
skip :: [a] -> Int -> [a]
skip xs = mapMaybe (atMay xs) . skipEvery (length xs)

skipEvery :: Int -> Int -> [Int]
skipEvery l n = [n, (n * 2 + 1) .. l - 1]

atMay :: [a] -> Int -> Maybe a
atMay [] _       = Nothing
atMay [x] 0      = Just x
atMay (x : _) 0  = Just x
atMay (_ : xs) i = atMay xs (i - 1)

-- * Exercise 2

localMaxima :: [Integer] -> [Integer]
localMaxima (x : y : z : zs)
  | y > x && y > z = y : localMaxima (y : z : zs)
localMaxima (_ : y : z : zs) = localMaxima (y : z : zs)
localMaxima _ = []

-- * Exercise 3

type HistogramData a = [a]

type HistogramPoints a = [[a]]

type HistogramRow a = [a]

histogram :: (Integral a) => HistogramData a -> String
histogram = flip (<>) histogramFooter . renderRows . dataPointsRows . parseDataPoints

histogramFooter :: String
histogramFooter =
  unlines
    [ "==========",
      "0123456789"
    ]

-- histogramLine :: (Integral a) => HistogramData a -> Int
-- histogramLine = foldr ((.|.) . bit . fromIntegral) zeroBits

parseDataPoints :: (Num a, Eq a, Ord a) => HistogramData a -> HistogramPoints a
parseDataPoints = group . sort

dataPointsRows :: (Ord a) => HistogramPoints a -> [HistogramRow a]
dataPointsRows xs = map (nthRow xs) [0 .. pointsDepth xs - 1]

nthRow :: (Eq a, Ord a) => HistogramPoints a -> Int -> HistogramRow a
nthRow xs n = mapMaybe (`atMay` n) xs

pointsDepth :: HistogramPoints a -> Int
pointsDepth = foldr (max . length) 0

rowToBits :: (Integral a) => HistogramRow a -> Int
rowToBits = foldr ((.|.) . bit . fromIntegral) zeroBits

renderRows :: (Integral a) => [HistogramRow a] -> String
renderRows = unlines . reverse . map renderRow

renderRow :: (Integral a) => HistogramRow a -> String
renderRow = bitsToHistogramStr . rowToBits

bitsToHistogramStr :: (Bits a) => a -> String
bitsToHistogramStr n = foldr (appendBit n) "" [0 .. 9]

appendBit :: (Bits a) => a -> Int -> String -> String
appendBit a n xs = bitToChar a n : xs

bitToChar :: Bits a => a -> Int -> Char
bitToChar xs n | testBit xs n = '*'
bitToChar _ _ = ' '
