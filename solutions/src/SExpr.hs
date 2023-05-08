module SExpr (
  -- * Exercise 1
  zeroOrMore,
  oneOrMore,
  -- * Exercise 2
  spaces,
  ident,
  -- * Exercise 3
  parseSExpr,
) where

import AParser             ()
import Control.Applicative ((<|>))
import Data.Char           (isAlpha, isAlphaNum, isSpace)
import Provided.AParser    (Parser, char, posInt, satisfy)

-- * Exercise 1

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p

-- * Exercise 2

spaces :: Parser String
spaces = zeroOrMore $ satisfy isSpace

ident :: Parser String
ident = (++) <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)

-- * Exercise 3

type Ident = String

data Atom
  = N Integer
  | I Ident
  deriving Show

data SExpr
  = A Atom
  | Comb [SExpr]
  deriving Show

parseAtom :: Parser SExpr
parseAtom = A <$> ((N <$> posInt) <|> (I <$> ident))

parseComb :: Parser SExpr
parseComb = spaces *> char '(' *> spaces *> (Comb <$> oneOrMore parseSExpr) <* spaces <* char ')' <* spaces

parseSExpr :: Parser SExpr
parseSExpr = spaces *> parseAtom <|> parseComb <* spaces
