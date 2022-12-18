{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day15 where

import Data.Char (isDigit)
import Data.Foldable
import Data.Function
import Data.List (nub)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Set as S
import Debug.Trace

type Position = (Int, Int)
type Range = (Int, Int)

data Scanner = Scanner {position :: Position, closestBeacon :: Position} deriving (Show, Eq)

type Scans = S.Set Position

solve :: [String] -> Int
solve xs = result
  where
    result = guenther numberOfPoints beaconXPositionsAtTarget
    numberOfPoints = nub . reduceRanges . guenther2 scanners $ 2000000

    beaconXPositionsAtTarget = nub . filter (\x -> snd x == 2000000) . map closestBeacon $ scanners
    scanners = parse xs

solvePartTwo :: [String] -> [[(Int, Int)]]
solvePartTwo xs = error ""

reduceRanges :: [(Int, Int)] -> [(Int, Int)]
reduceRanges ranges = go ranges []
  where
    go [] result = result
    go ((from, to) : xs) result
      | null overlaps = go xs ((from, to) : result)
      | otherwise = go xs nextResult
      where
        nextResult = (minX, maxX) : filter (\(f, t) -> not (from <= t && f <= to || (from == t + 1))) result
        minX = minimum [minimum . map fst $ overlaps, from]
        maxX = maximum [maximum . map snd $ overlaps, to]

        overlaps = filter (\(f, t) -> from <= t && f <= to || (from == t + 1)) result

overlaps2 :: Position -> Position -> Bool
overlaps2 (x, y) (x', y') = x <= y' && x' <= y

guenther2 :: [Scanner] -> Int -> [Range]
guenther2 [] _ = []
guenther2 (scanner : xs) targetY = case scanningPositions scanner targetY of
  Just a -> a : guenther2 xs targetY
  Nothing -> guenther2 xs targetY

guenther :: [Range] -> [Position] -> Int
guenther [] _ = 0
guenther ((from, to) : xs) beaconXPositions
  | numberOfBeaconsBetween == 0 = to - from + 1 + guenther xs beaconXPositions
  | otherwise = to - from + 1 - numberOfBeaconsBetween + guenther xs beaconXPositions
  where
    numberOfBeaconsBetween = length . filter (overlaps2 (from, to)) $ beaconXPositions

manhattenDistance :: Position -> Position -> Int
manhattenDistance (x, y) (x', y') = abs (y' - y) + abs (x' - x)

parse :: [String] -> [Scanner]
parse [] = []
parse (x : xs) = extractScanner . splitOn ":" $ x
  where
    extractScanner raw = Scanner {position = position, closestBeacon = beacon} : parse xs

    position = extractPosition . head $ splitted
    beacon = extractPosition . last $ splitted
    splitted = splitOn ":" x

extractPosition :: String -> Position
extractPosition xs = (x, y)
  where
    x = readNumber . init . last . splitOn "=" . last . init $ splitted
    y = readNumber . last . splitOn "=" . last $ splitted
    splitted = splitOn " " xs

readNumber :: String -> Int
readNumber ('-':xs) = read xs * (-1)
readNumber xs = read xs

scanningPositions :: Scanner -> Int -> Maybe Range
scanningPositions Scanner {position = (x, y), closestBeacon = (bx, by)} targetY
  | distance - distanceY < 0 = Nothing
  | otherwise = Just (x - (distance - distanceY), x + (distance - distanceY))
  where
    distanceY = abs (y - targetY)
    distance = manhattenDistance (x, y) (bx, by)
