module Main where

import Criterion (bench, bgroup, whnf)
import Criterion.Main (defaultMain)
import Data.Bits (shiftL)
import Lib (toDigits)
import System.Random (randomRIO)

main :: IO ()
main = do
  [b1, b2, b3, b4, b5, b6] <- mapM randomInteger [1, 10, 100, 1000, 10000, 100000]
  defaultMain
    [ bgroup
        "toDigits"
        [ bench "1 digits" $ whnf toDigits b1,
          bench "2 digits" $ whnf toDigits b2,
          bench "3 digits" $ whnf toDigits b3,
          bench "4 digits" $ whnf toDigits b4,
          bench "5 digits" $ whnf toDigits b5,
          bench "6 digits" $ whnf toDigits b6
        ]
    ]

randomInteger :: Integer -> IO Integer
randomInteger = curry randomRIO <$> id <*> (subtract 1 . upperBound)

upperBound :: Integer -> Integer
upperBound = (+) <$> (`shiftL` 1) <*> (`shiftL` 3)
