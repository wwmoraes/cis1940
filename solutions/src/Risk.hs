module Risk (
  -- * Exercise 1
  -- | Nothing to do here. The homework assigns you to install the @MonadRandom@
  -- package using cabal directly. We instead have it as a dependency. You're
  -- done if you can build this module using stack.

  -- * Exercise 2
  battle,

  -- * Exercise 3
  invade,

  -- * Exercise 4
  successProb,

  -- * Exercise 5
  exactSuccessProb,
) where

import Control.Monad.Random
import Data.Bifunctor       (Bifunctor (bimap))
import Data.Functor         ((<&>))
import Data.List            (partition, sort)
import Data.Ratio           ((%))
import Provided.Risk

-- * Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle bf
  | attackers bf <= 0 = return bf
  | defenders bf <= 0 = return bf
battle bf = do
  let atk = attackers bf
  let def = defenders bf

  atkRolls <- replicateM (atk-1) die <&> sort
  defRolls <- replicateM def die <&> sort

  let pairs = zip atkRolls defRolls
  let loss = bimap length length $ partition (uncurry (>)) pairs

  return Battlefield
    { attackers = max 0 atk - snd loss
    , defenders = max 0 def - fst loss
    }

-- * Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf <= 1 = return bf
  | defenders bf <= 0 = return bf
invade bf = battle bf >>= invade

-- * Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb bf = do
  results <- replicateM 1000 $ invade bf

  let wins = filter ((== 0) . defenders) results

  return $ fromRational $ length' wins % length' results

-- * Exercise 5

exactSuccessProb :: Battlefield -> Double
exactSuccessProb = undefined

-- singleAttackProb :: Double
-- singleAttackProb = fromRational $ fst winLoss % uncurry (+) winLoss
--   where
--     sides = [1..6] :: [Integer]
--     possibilities = (,) <$> sides <*> sides
--     winLoss = bimap length' length' $ partition (uncurry (>)) possibilities

-- * Extra

length' :: Traversable t => t a -> Integer
length' = fromIntegral . length
