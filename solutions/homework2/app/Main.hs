{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Main (main) where

import LogAnalysis  (parse, whatWentWrong)
import Provided.Log (testParse, testWhatWentWrong)

import Paths_cis1940_solutions (getDataFileName)

main :: IO ()
main = do
  putStrLn "testParse"
  getDataFileName "error.log" >>= testParse parse 10 >>= mapM_ print
  putStrLn "testWhatWentWrong"
  getDataFileName "sample.log" >>= testWhatWentWrong parse whatWentWrong >>= mapM_ print
