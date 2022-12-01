module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day1 (solvePartTwo)
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day1.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
