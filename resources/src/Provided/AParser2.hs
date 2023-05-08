{-# OPTIONS_GHC -Wno-orphans #-}

module Provided.AParser2 (Parser, runParser, satisfy, char, posInt) where

import Control.Applicative
import Provided.AParser

inParser :: ((String -> Maybe (a, String)) -> String -> Maybe (b, String)) -> Parser a -> Parser b
inParser f = Parser . f . runParser

first :: (a -> b) -> (a,c) -> (b,c)
first f (x,y) = (f x, y)

instance Functor Parser where
  fmap = inParser . fmap . fmap . first

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (Parser fp) <*> xp = Parser $ \s ->
    case fp s of
      Nothing     -> Nothing
      Just (f,s') -> runParser (f <$> xp) s'

instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
