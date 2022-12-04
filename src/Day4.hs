module Day4 where

import Data.List.Split (splitOn)

type From = Int

type To = Int

type Assignment = (From, To)

solve :: [String] -> Int
solve = length . filter (== True) . map (fullyOverlaps . convertToAssignmentPair)

solvePartTwo :: [String] -> Int
solvePartTwo = length . filter (== True) . map (partiallyOverlaps . convertToAssignmentPair)

convertToAssignmentPair :: String -> (Assignment, Assignment)
convertToAssignmentPair xs = (firstAssignment, secondAssingment)
  where
    [first, second] = splitOn "," xs
    firstAssignment = convertToAssignment first
    secondAssingment = convertToAssignment second

convertToAssignment :: String -> Assignment
convertToAssignment xs = (from, to)
  where
    from = read . head . splitOn "-" $ xs
    to = read . last . splitOn "-" $ xs

fullyOverlaps :: (Assignment, Assignment) -> Bool
fullyOverlaps (x, y) = x `within` y || y `within` x

partiallyOverlaps :: (Assignment, Assignment) -> Bool
partiallyOverlaps (x, y) = x `partiallyWithin` y

within :: Assignment -> Assignment -> Bool
within (x, y) (x', y') = x >= x' && y <= y'

partiallyWithin :: Assignment -> Assignment -> Bool
partiallyWithin (x, y) (x', y') = x <= y' && x' <= y
