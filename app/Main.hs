module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day3
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day3.txt" ReadMode
  contents <- hGetContents handle
  print $ solvePartTwo . lines $ contents
