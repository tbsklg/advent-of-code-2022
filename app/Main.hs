module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day15
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day15.txt" ReadMode
  contents <- hGetContents handle
  -- print $ solve $ lines contents
  print $ solvePartTwo $ lines contents
