{-# OPTIONS_GHC -Wno-orphans #-}
-- |
-- Module      : Party
-- Description : Homework 8 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for the homework 8 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>

module Party (
  -- * Exercise 1
  glCons,
  moreFun,
  -- * Exercise 2
  treeFold,
  -- * Exercise 3
  nextLevel,
  -- * Exercise 4
  maxFun,
  -- * Exercise 5
  -- | __'main' is implemented in the homework8 binary__. The functions here are
  -- pure transformations to reduce the imports that'd otherwise leak into the
  -- imperative shell.
  parseHierarchy,
  glFun,
  glGuests,
) where

import Data.Tree (Tree (Node))

import Data.Foldable (fold)

import Provided.Employee

-- * Exercise 1

-- | includes an 'Employee' in the 'GuestList'
glCons :: Employee -> GuestList -> GuestList
glCons a (GL as b) = GL (a:as) (b + empFun a)

-- | merges the @['Employee']@ lists and sums up the 'Fun' score
instance Semigroup GuestList where
  (<>) (GL as s1) (GL bs s2) = GL (as ++ bs) (s1 + s2)

-- | creates a 'GuestList' with an empty @['Employee']@ and zero 'Fun'
instance Monoid GuestList where
  mempty = GL [] 0

-- | returns the 'GuestList' with the highest 'Fun' score, or the first one
-- if they're equal
moreFun :: GuestList -> GuestList -> GuestList
moreFun (GL _ af) b@(GL _ bf)
  | af < bf = b
moreFun a _ = a

-- * Exercise 2

-- treeFoldr :: (Monoid a, Monoid b) => (a -> b -> b) -> b -> Tree a -> b
-- treeFoldr f z (Node a []) = f a z
-- treeFoldr f z (Node a as) = foldMap (treeFoldr f (f a z)) as

-- treeFoldMap :: Monoid b => (a -> b) -> Tree a -> b
-- treeFoldMap f (Node a []) = f a
-- treeFoldMap f (Node a as) = f a <> foldMap (treeFoldMap f) as

-- | specialized version of 'foldMap' for 'Tree'
treeFold :: Monoid b => (a -> b) -> Tree a -> b
treeFold f (Node a []) = f a
treeFold f (Node a as) = f a <> foldMap (treeFold f) as

-- * Exercise 3

-- | calculates the best 'GuestList' with and without an 'Employee'. The list
-- provided should contain pairs of the employee direct subtrees, where the
-- first 'GuestList' includes the root of that subtree, and the second doesn't.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e = ((,) <$> fold . fst <*> glCons e . fold . snd) . unzip

-- * Exercise 4

-- | transforms a @'Tree' Employee'@ into a 'GuestList' where the fun score is
-- the maximum possible. The constraint is that the fun score of a particular
-- 'Employee' becomes zero if their direct parent is also in the final list.
maxFun :: Tree Employee -> GuestList
maxFun (Node e []) = empGuest e
-- maxFun (Node e ts) = uncurry moreFun $ nextLevel e (map glFunOptions ts)
maxFun (Node e ts) = uncurry moreFun $ nextLevel e (map glFunOptions ts)

-- * Extra

glFunOptions :: Tree Employee -> (GuestList, GuestList)
glFunOptions (Node e []) = (empGuest e, mempty)
glFunOptions (Node e ts) = (glCons e without, with)
  where (with, without) = foldMap glFunOptions ts

-- | read a raw 'String' into a @'Tree' 'Employee'@
parseHierarchy :: String -> Tree Employee
parseHierarchy = read

-- | returns the 'Fun' score of a 'GuestList'
glFun :: GuestList -> Fun
glFun (GL _ s) = s

-- | returns each 'Employee'\'s 'Name' within the 'GuestList'
glGuests :: GuestList -> [Name]
glGuests (GL gs _) = map empName gs

-- | lifts an 'Employee' into a 'GuestList'
empGuest :: Employee -> GuestList
empGuest = GL <$> return <*> empFun
