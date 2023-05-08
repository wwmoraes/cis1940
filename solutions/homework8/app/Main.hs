module Main where

import Party (glFun, glGuests, maxFun, parseHierarchy)

import Provided.Employee (GuestList)

import Paths_cis1940_solutions (getDataFileName)

main :: IO ()
main = getDataFileName "company.txt" >>= fileMaxFun >>= reportGuestList

-- parse the raw 'String' as a @'Tree' 'Employee'@ and calculate the 'maxFun'
-- out of it
parseMaxFun :: String -> GuestList
parseMaxFun = maxFun . parseHierarchy

-- read a file content and parse
fileMaxFun :: FilePath -> IO GuestList
fileMaxFun = fmap parseMaxFun . readFile

reportGuestList :: GuestList -> IO ()
reportGuestList gl = do
  putStrLn $ "Total fun: " ++ show (glFun gl)
  mapM_ putStrLn $ glGuests gl
