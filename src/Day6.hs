module Day6 where

import Data.List ( nub )
import Data.List.Split ( divvy )

solve :: String -> Int
solve = findMarker

solvePartTwo :: String -> Int
solvePartTwo = findMessageMarker

findMarker :: (Num c, Enum c) => [Char] -> c
findMarker = fst . last . head . dropWhile (not . bla) . divvy 4 1 . zip [1 ..]
    where
        bla ys = containsMarker . map snd $ ys

findMessageMarker :: (Num c, Enum c) => [Char] -> c
findMessageMarker = fst . last . head . dropWhile (not . bla) . divvy 14 1 . zip [1 ..]
    where
        bla ys = containsMarker . map snd $ ys

containsMarker :: String -> Bool
containsMarker xs = nub xs == xs
