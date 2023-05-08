{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- originally StringBufEditor.hs

module Main where

import JoinList ()
import Provided.Buffer (fromString)
import Provided.Editor (editor, runEditor)
import Provided.JoinList (JoinList)
import Provided.Sized (Size)
import Scrabble (Score)

-- import Provided.StringBuffer ()

main :: IO ()
main =
  runEditor editor . load $
    unlines
      [ "This buffer is for notes you don't want to save, and for",
        "evaluation of steam valve coefficients.",
        "To load a different file, type the character L followed",
        "by the name of the file."
      ]

load :: String -> JoinList (Score, Size) String
load = fromString
