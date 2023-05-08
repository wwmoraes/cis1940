{-# OPTIONS -Wno-orphans #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : JoinList
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
module JoinList
  ( -- * Exercise 1
    (+++),
    tag,

    -- * Exercise 2
    indexJ,
    dropJ,
    takeJ,

    -- * Exercise 3
    scoreLine,

    -- * Exercise 4

    -- | implemented in the homework7 binary
  )
where

import Provided.Buffer (Buffer (fromString, line, numLines, replaceLine, toString, value))
import Provided.JoinList (JoinList (..), jlToList, (!!?))
import Provided.Sized (Size (Size), Sized, getSize, size)
import Scrabble (Score, getScore, scoreString)

-- * Exercise 1

-- | appends two 'JoinList' values. The new metadata is the associative result
-- of both values.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (tag x <> tag y) x y

-- | extracts the metadata of a 'JoinList'
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- * Exercise 2

-- | returns the 'JoinList' size based on its 'Sized' metadata
sizeJ :: (Sized m, Monoid m) => JoinList m a -> Int
sizeJ = getSize . size . tag

-- | indexes a 'JoinList' and returns the value found if any
indexJ :: (Sized m, Monoid m) => Int -> JoinList m a -> Maybe a
indexJ _ Empty = Nothing
indexJ i _ | i < 0 = Nothing
indexJ i jl = jlToList jl !!? i

-- | specialized 'drop' for 'JoinList'
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n x | n <= 0 = x
dropJ _ (Single _ _) = Empty
dropJ n x | n >= sizeJ x = Empty
dropJ n (Append _ l r) = dropJ ln l +++ dropJ lr r
  where
    ln = min n (sizeJ l)
    lr = n - ln

-- | specialized 'take' for 'JoinList'
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n _ | n <= 0 = Empty
takeJ _ x@(Single _ _) = x
takeJ n x | n >= sizeJ x = x
takeJ n (Append _ l r) = takeJ ln l +++ takeJ lr r
  where
    ln = min n (sizeJ l)
    lr = n - ln

-- * Exercise 3

-- | calculates the Scrabble score and lifts the text into a
-- @'JoinList' 'Score'@ context
scoreLine :: String -> JoinList Score String
scoreLine = Single <$> scoreString <*> id

-- Exercise 4

-- | calculates the 'Score' and 'Size' of a text
scoreSizeOf :: String -> (Score, Size)
scoreSizeOf = curry id <$> scoreString <*> const (Size 1)

-- | calculates the Scrabble score, the size and lifts the text into a
-- @'JoinList' ('Score', 'Size')@ context
scoreSizeLine :: String -> JoinList (Score, Size) String
scoreSizeLine = Single <$> scoreSizeOf <*> id

-- | make @'JoinList' ('Score', 'Size')@ a 'Foldable' to allow easy string
-- generation
instance Foldable (JoinList (Score, Size)) where
  foldMap :: Monoid b => (a -> b) -> JoinList (Score, Size) a -> b
  foldMap _ Empty = mempty
  foldMap f (Single _ x) = f x
  foldMap f (Append _ l r) = foldMap f l <> foldMap f r

-- | make @'JoinList' ('Score', 'Size')@ a 'Buffer' to enable its use with the
-- editor from "Provided.Editor"
instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty = ""
  toString (Single _ x)  = x ++ "\n"
  toString (Append _ l r) = toString l ++ toString r
  -- toString = concat

  fromString :: String -> JoinList (Score, Size) String
  fromString = foldr ((+++) . scoreSizeLine) Empty . lines

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine :: Int -> String -> JoinList (Score, Size) String -> JoinList (Score, Size) String
  replaceLine n s jl = takeJ n jl +++ scoreSizeLine s +++ dropJ (n + 1) jl

  numLines :: JoinList (Score, Size) String -> Int
  numLines Empty = 0
  numLines (Single _ _) = 1
  numLines (Append _ l r) = numLines l + numLines r

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag
