{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}

module AParser (
  -- * Exercise 1
  -- | * @'Functor' 'Parser'@ instance
  first,

  -- * Exercise 2
  -- | * @'Applicative' 'Parser'@ instance

  -- * Exercise 3
  abParser,
  abParser_,
  intPair,

  -- * Exercise 4
  -- | * @'Alternative' 'Parser'@ instance

  -- * Exercise 5
  intOrUppercase,
) where

import Control.Applicative (Alternative (..), Applicative (liftA2))
import Control.Monad       (void)
import Data.Char           (isAsciiUpper)
import Provided.AParser

-- * Exercise 1

instance Functor Parser where
  fmap f pa = Parser $ fmap (first f) . runParser pa

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

-- * Exercise 2

instance Applicative Parser where
  pure x = Parser $ Just . (,) x
  pf <*> pa = Parser $ \s ->
    case runParser pf s of
      Nothing      -> Nothing
      Just (f, sf) -> runParser (fmap f pa) sf
  -- (<*>) pf pa = Parser $ chain pa . runParser pf
  --   where
  --     chain _ Nothing        = Nothing
  --     chain a (Just (f, sf)) = runParser (fmap f a) sf

-- * Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Integer]
intPair =  (\x _ y -> [x,y]) <$> posInt <*> void (char ' ') <*> posInt

-- * Exercise 4

instance Alternative Parser where
  empty = Parser $ const Nothing
  Parser a <|> Parser b = Parser $ liftA2 (<|>) a b

-- * Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isAsciiUpper)

-- * Extra
