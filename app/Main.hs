module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day16 (solve, solvePartTwo)
import System.CPUTime

main :: IO ()
main = do
  -- startTime <- getCPUTime
  solve $ readFile "app/resources/day16.txt"
  solvePartTwo $ readFile "app/resources/day16.txt"
  -- endTime <- getCPUTime
  -- let elapsedTime = fromIntegral (endTime - startTime) / (10^9)
  -- putStrLn ("Elapsed time: " ++ show elapsedTime ++ " milliseconds")
