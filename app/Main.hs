module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day6
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day6.txt" ReadMode
  contents <- hGetContents handle
  print $ solve contents
  print $ solvePartTwo contents
