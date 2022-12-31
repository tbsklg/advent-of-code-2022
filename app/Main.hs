module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day16 (solve)
import System.CPUTime

main :: IO ()
main = do
  handle <- openFile "app/resources/day16.txt" ReadMode
  contents <- hGetContents handle
  -- print $ solve $ lines contents
  -- startTime <- getCPUTime
  solve $ readFile "app/resources/day16.txt"
  -- endTime <- getCPUTime
  -- let elapsedTime = fromIntegral (endTime - startTime) / (10^9)
  -- putStrLn ("Elapsed time: " ++ show elapsedTime ++ " milliseconds")
