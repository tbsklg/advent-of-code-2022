module Day2 where

import Data.List.Split (splitOn)
import Day1 (solvePartTwo)
import qualified Data.Bifunctor

data RPS = Rock | Paper | Scissor deriving Eq
data Result = Win | Loss | Draw deriving Eq

solve :: [String] -> Int
solve = foldl (\y x -> y + game x) 0 . map (Data.Bifunctor.bimap toRPS toRPS . (\x -> (head x, last x)))

solvePartTwo :: [String] -> Int
solvePartTwo = sum . map (pointsB . (\x -> (head x, last x)))

-- (1 for Rock, 2 for Paper, and 3 for Scissors)
-- (0 if you lost, 3 if the round was a draw, and 6 if you won)
-- X loose, Y means you need to end the round in a draw, and Z means you need to win.

game match@(opponent, me) = pointsRPS me + (pointsC . points $ match)

toRPS :: Char -> RPS
toRPS 'A' = Rock
toRPS 'X' = Rock
toRPS 'B' = Paper
toRPS 'Y' = Paper
toRPS 'C' = Scissor
toRPS 'Z' = Scissor
toRPS _ = error ""

pointsC :: Result -> Int
pointsC Win = 6
pointsC Draw = 3
pointsC Loss = 0

pointsRPS :: Num p => RPS -> p
pointsRPS Rock = 1
pointsRPS Paper = 2
pointsRPS Scissor = 3

points :: (RPS, RPS) -> Result
points (Rock, Rock) = Draw
points (Rock, Paper) = Win
points (Rock, Scissor) = Loss
points (Paper, Rock) = Loss
points (Paper, Paper) = Draw
points (Paper, Scissor) = Win
points (Scissor, Rock) = Win
points (Scissor, Paper) = Loss
points (Scissor, Scissor) = Draw

-- points :: (Char, Char) -> Int
-- points (Rock, Rock) = 4
-- points (Rock, Paper) = 8
-- points (Rock, Scissor) = 3
-- points (Paper, Rock) = 1
-- points (Paper, Paper) = 5
-- points (Paper, Scissor) = 9
-- points (Scissor, Rock) = 7
-- points (Scissor, Paper) = 2
-- points (Scissor, Scissor) = 6
-- points _ = 0

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
