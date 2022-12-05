module Day5 where

import Data.List.Split (divvy, splitOn)

data Move = Move {numberOfCrates :: Int, from :: Int, to :: Int} deriving (Show, Eq)

solve :: [String] -> [String]
solve xs = xs

crates :: [String] -> [[String]]
crates = group . map (divvy 1 4 . tail)
  where
    group [] = []
    group xs
      | all null xs = []
      | otherwise = map head xs : (group . map tail $ xs)

moves :: [String] -> [Move]
moves = map move

move :: String -> Move
move xs =
  Move
    { numberOfCrates = read . (!! 1) . splitOn " " $ xs,
      from = read . (!! 3) . splitOn " " $ xs,
      to = read . (!! 5) . splitOn " " $ xs
    }
