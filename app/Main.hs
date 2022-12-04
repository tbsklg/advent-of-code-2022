module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day4
  
main :: IO ()
main = do
  handle <- openFile "app/resources/day4.txt" ReadMode
  contents <- hGetContents handle
  print $ solve . lines $ contents
