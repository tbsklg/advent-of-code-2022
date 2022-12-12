module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day11 ( solve, solvePartTwo )
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day11.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  print $ solvePartTwo $ lines contents
  -- putStr $ solvePartTwo $ lines contents
