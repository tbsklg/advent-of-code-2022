module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day8
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day8.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  print $ solvePartTwo $ lines contents
