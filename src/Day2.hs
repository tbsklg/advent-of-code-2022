module Day2 where

import Data.List.Split (splitOn)
import Day1 (solvePartTwo)

solve :: [String] -> Int
solve = sum . map (points . (\x -> (head x, last x)))

solvePartTwo :: [String] -> Int
solvePartTwo = sum . map (pointsB . (\x -> (head x, last x)))

-- (1 for Rock, 2 for Paper, and 3 for Scissors)
-- (0 if you lost, 3 if the round was a draw, and 6 if you won)
-- X loose, Y means you need to end the round in a draw, and Z means you need to win.

pointsB :: (Char, Char) -> Int
pointsB ('A', 'X') = 3
pointsB ('A', 'Y') = 4
pointsB ('A', 'Z') = 8
pointsB ('B', 'X') = 1
pointsB ('B', 'Y') = 5
pointsB ('B', 'Z') = 9
pointsB ('C', 'X') = 2
pointsB ('C', 'Y') = 6
pointsB ('C', 'Z') = 7
pointsB _ = 0

points :: (Char, Char) -> Int
points ('A', 'X') = 4
points ('A', 'Y') = 8
points ('A', 'Z') = 3
points ('B', 'X') = 1
points ('B', 'Y') = 5
points ('B', 'Z') = 9
points ('C', 'X') = 7
points ('C', 'Y') = 2
points ('C', 'Z') = 6
points _ = 0
