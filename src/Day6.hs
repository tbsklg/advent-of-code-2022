module Day6 where

import Data.List (nub)
import Data.List.Split (divvy)

solve :: String -> Int
solve = findMarker 4

solvePartTwo :: String -> Int
solvePartTwo = findMarker 14

findMarker :: Int -> String -> Int
findMarker i = fst . last . head . dropWhile (not . containsMarker) . divvy i 1 . zip [1 ..]
  where
    containsMarker ys = isUnique . map snd $ ys

isUnique :: String -> Bool
isUnique xs = nub xs == xs
