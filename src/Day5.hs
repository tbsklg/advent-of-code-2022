module Day5 where
import Data.List.Split (splitOn, divvy)

solve :: [String] -> [String]
solve xs = xs

crates :: [String] -> [[String]]
crates = group . map (divvy 1 4 . tail)
    where
        group [] = []
        group xs
            | all null xs = []
            | otherwise  = map head xs : (group . map tail $ xs)
