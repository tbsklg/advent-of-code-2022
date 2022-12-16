module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day14
import Data.List (intercalate, maximumBy)
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day14.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  putStrLn $ intercalate "\n" . furtherFallings . grid (500,0) . parse $ lines contents
  -- print $ solvePartTwo $ lines contents
