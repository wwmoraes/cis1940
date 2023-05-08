{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      : LogAnalysis
-- Description : Homework 2 solution
-- Copyright   : (c) William Artero, 2023
-- License     : MIT
-- Maintainer  : haskell@artero.dev
-- Stability   : experimental
-- Portability : POSIX
--
-- Solution for homework 2 of the CIS 194 lecture
--
-- Lecture home page: <https://www.cis.upenn.edu/~cis1940/spring13/>
module LogAnalysis
  ( -- * Exercise 1
    parseMessage,
    parse,

    -- * Exercise 2
    insert,

    -- * Exercise 3
    build,

    -- * Exercise 4
    inOrder,

    -- * Exercise 5
    whatWentWrong,
    whatWentWrong',

    -- * Extras
    message,
  )
where

import Control.Monad (join)
import Data.Maybe    (fromMaybe)
import Provided.Log
import Text.Read     (readMaybe)

-- * Exercise 1

-- | parses a single log line into a 'LogMessage'
parseMessage :: String -> LogMessage
parseMessage l = fromMaybe (Unknown l) (parseMessageWords $ words l)

-- | parses multiple logs separated by newlines into 'LogMessage' entries
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- | parses a log line words into a 'LogMessage'. Yields 'Nothing' if the
-- log line is malformed.
parseMessageWords :: [String] -> Maybe LogMessage
parseMessageWords ("I" : t : xs) = do
  tt <- readMaybe t :: Maybe TimeStamp
  return . LogMessage Info tt $ unwords xs
parseMessageWords ("W" : t : xs) = do
  tt <- readMaybe t :: Maybe TimeStamp
  return . LogMessage Warning tt $ unwords xs
parseMessageWords ("E" : n : t : xs) = do
  nn <- readMaybe n :: Maybe Int
  tt <- readMaybe t :: Maybe TimeStamp
  return . LogMessage (Error nn) tt $ unwords xs
parseMessageWords xs = return . Unknown $ unwords xs

-- * Exercise 2

-- | inserts a 'LogMessage' into the 'MessageTree'. Ignores 'Unknown' logs.
insert :: LogMessage -> MessageTree -> MessageTree
-- skip unknown log entries
insert (Unknown _) mt = mt
-- replace an unknown node. Only set to make the function complete.
-- it should never happen unless it receives a hand-crafted tree, in which case
-- it neither guarantees order nor balance
insert nlm (Node l (Unknown _) r) = Node l nlm r
-- add new leaf node
insert nlm Leaf = Node Leaf nlm Leaf
-- recursive insertion in the right subtree
insert nlm@(LogMessage _ nts _) (Node lmt m@(LogMessage _ ts _) rmt)
  | nts < ts = Node (insert nlm lmt) m rmt
  | nts > ts = Node lmt m (insert nlm rmt)
insert _ mt = mt

-- * Exercise 3

-- | creates a new 'MessageTree' for a list of 'LogMessage' items
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- * Exercise 4

-- | unfolds a 'MessageTree' into an ordered list of 'LogMessage' items
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf          = []
inOrder (Node l lm r) = join [inOrder l, lm : inOrder r]

-- * Exercise 5

-- | filters out the logs and returns only errors with a severity of 50 or more
-- (pattern-matching style)
whatWentWrong' :: [LogMessage] -> [String]
whatWentWrong' [] = []
whatWentWrong' ((LogMessage (Error n) _ s) : xs)
  | n >= 50 = s : whatWentWrong' xs
whatWentWrong' (_ : xs) = whatWentWrong' xs

-- | filters out the logs and returns only errors with a severity of 50 or more
-- (point-free style)
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map message . filter ((>= 50) . severity)

-- EXTRA

severity :: LogMessage -> Int
severity (LogMessage (Error n) _ _) = n
severity _                          = 0

-- | retrieves the log message content if it was parsed successfully or the
-- entire log line entry if it is an 'Unknown'
message :: LogMessage -> String
message (LogMessage _ _ s) = s
message (Unknown s)        = s
