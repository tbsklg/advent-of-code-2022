module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day7
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day7.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  -- print $ solvePartTwo contents
