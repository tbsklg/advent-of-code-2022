module Main where

import System.IO (IOMode (ReadMode), hGetContents, openFile)
import Day16 (solve, solvePartTwo)
import System.CPUTime ( getCPUTime )

main :: IO ()
main = do
  startTime <- getCPUTime
  solve $ readFile "app/resources/day16.txt"
  endTime <- getCPUTime
  startTime2 <- getCPUTime
  solvePartTwo $ readFile "app/resources/day16.txt"
  endTime2 <- getCPUTime
  let elapsedTime = fromIntegral (endTime - startTime) / (10^9)
  let elapsedTime2 = fromIntegral (endTime2 - startTime2) / (10^9)
  putStrLn ("Elapsed time: " ++ show elapsedTime ++ " milliseconds")
  putStrLn ("Elapsed time: " ++ show elapsedTime2 ++ " milliseconds")
