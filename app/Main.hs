module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day9 ( solve, solvePartTwo )
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day9.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  print $ solvePartTwo $ lines contents
