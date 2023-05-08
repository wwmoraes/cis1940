{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : HOC
-- Description : Homework 4 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 4 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module HOC
  ( -- * Exercise 1
    fun1',
    fun2',

    -- * Exercise 2
    foldTree,

    -- * Exercise 3
    xor,
    map',

    -- * Exercise 4
    sieveSundaram,
  )
where

import Data.List ((\\))

-- * Exercise 1

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (> 0) . iterate next
  where
    next 1 = 0
    next n | even n = n `div` 2
    next n = 3 * n + 1

-- * Exercise 2

data Tree a
  = Node Integer (Tree a) a (Tree a)
  | Leaf
  deriving (Show)

foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node h l v r)
  | hl < hr = Node h (insert x l) v r
  | hl > hr = Node h l v nr
  | otherwise = Node (hn + 1) l v nr
  where
    hl = height l
    hr = height r
    nr = insert x r
    hn = height nr

height :: Tree a -> Integer
height Leaf           = -1
height (Node h _ _ _) = h

-- showTree :: Show a => Tree a -> String
-- showTree Leaf = ""
-- showTree n@(Node j _ _ _) = go j n
--   where
--     go _ (Leaf) = ""
--     go i (Node _ l c r) =
--       go (i - 1) l
--         ++ replicate (4 * fromIntegral i) ' '
--         ++ show c
--         ++ "\n"
--         ++ go (i - 1) r

-- * Exercise 3

xor :: [Bool] -> Bool
xor = foldr (const not) False . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

-- * Exercise 4

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((1 +) . (2 *)) $ [1 .. n] \\ sieves
  where
    products = cartesianProduct [1 .. n] [1 .. n]
    sieve = (+) <$> uncurry (+) <*> (2 *) . uncurry (*)
    sieves = filter (<= n) . map sieve $ products

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]
