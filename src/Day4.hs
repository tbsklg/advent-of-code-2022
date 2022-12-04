module Day4 where

import Data.List.Split (splitOn)

type From = Int

type To = Int

type Assignment = (From, To)

solve :: [String] -> Int
solve = length . filter (==True) . map (overlap . convertToAssignmentPair)

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

overlap :: (Assignment, Assignment) -> Bool
overlap (x,y) = x `within` y || y `within` x

within :: Assignment -> Assignment -> Bool
within (x, y) (x', y') = x >= x' && y <= y'
