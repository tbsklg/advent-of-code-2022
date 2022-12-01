module Day1 where

import Data.List (sort)

solve :: [String] -> Int
solve = maximum . carriageByElf

solvePartTwo :: [String] -> Int
solvePartTwo = sum . take 3 . reverse . sort . carriageByElf

carriageByElf :: (Num a, Read a) => [[Char]] -> [a]
carriageByElf [] = []
carriageByElf ("" : xs) = carriageByElf xs
carriageByElf xs = (sum . map read . takeWhile (/= "") $ xs) : (carriageByElf . dropWhile (/= "") $ xs)
