module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day10
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day10.txt" ReadMode
  contents <- hGetContents handle
  print $ solve $ lines contents
  putStr $ solvePartTwo $ lines contents
