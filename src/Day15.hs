module Day15 where

import Data.List (nub, sort)
import Data.List.Split (splitOn)

type Position = (Int, Int)

type Range = (Int, Int)

data Scanner = Scanner {position :: Position, closestBeacon :: Position} deriving (Show, Eq)

solve :: [String] -> Int
solve xs = numberOfScannedPositions numberOfPoints beaconsAtTargetLine
  where
    numberOfPoints = nub . reduceRanges . allScanningRanges scanners $ 10
    beaconsAtTargetLine = nub . filter (\x -> snd x == 10) . map closestBeacon $ scanners

    scanners = parse xs

solvePartTwo :: [String] -> Maybe Int
solvePartTwo xs = findBeaconAt [0 .. 4000000]
  where
    findBeaconAt [] = Nothing
    findBeaconAt (row : rs) = case findBeacon . numberOfPoints $ row of
      Just a -> Just (a * 4000000 + row)
      Nothing -> findBeaconAt rs

    numberOfPoints row = nub . reduceRanges . allScanningRanges scanners $ row
    scanners = parse xs

findBeacon :: [(Int, Int)] -> Maybe Int
findBeacon [] = Nothing
findBeacon [range] = Nothing
findBeacon [(from, to), (nextFrom, nextTo)]
  | nextFrom - to == 2 = Just (nextFrom - 1)
  | otherwise = Nothing
findBeacon ((from, to) : (nextFrom, nextTo) : rs)
  | nextFrom - to == 2 = Just (nextFrom - 1)
  | otherwise = findBeacon rs

reduceRanges :: [(Int, Int)] -> [(Int, Int)]
reduceRanges ranges = go ranges []
  where
    go [] result = sort result
    go ((from, to) : xs) result
      | null overlaps = go xs ((from, to) : result)
      | otherwise = go xs nextResult
      where
        nextResult = (minX, maxX) : filter (\(f, t) -> not (from <= t && f <= to || (from == t + 1))) result
        minX = minimum [minimum . map fst $ overlaps, from]
        maxX = maximum [maximum . map snd $ overlaps, to]

        overlaps = filter (\(f, t) -> from <= t && f <= to || (from == t + 1)) result

overlaps :: Position -> Position -> Bool
overlaps (x, y) (x', y') = x <= y' && x' <= y

allScanningRanges :: [Scanner] -> Int -> [Range]
allScanningRanges [] _ = []
allScanningRanges (scanner : xs) targetY = case scanningRange scanner targetY of
  Just a -> a : allScanningRanges xs targetY
  Nothing -> allScanningRanges xs targetY

numberOfScannedPositions :: [Range] -> [Position] -> Int
numberOfScannedPositions [] _ = 0
numberOfScannedPositions ((from, to) : xs) beaconPositions
  | overlappingBeacons == 0 = to - from + 1 + numberOfScannedPositions xs beaconPositions
  | otherwise = to - from + 1 - overlappingBeacons + numberOfScannedPositions xs beaconPositions
  where
    overlappingBeacons = length . filter (overlaps (from, to)) $ beaconPositions

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
readNumber ('-' : xs) = read xs * (-1)
readNumber xs = read xs

scanningRange :: Scanner -> Int -> Maybe Range
scanningRange Scanner {position = (x, y), closestBeacon = (bx, by)} targetY
  | distance - distanceY < 0 = Nothing
  | otherwise = Just (x - (distance - distanceY), x + (distance - distanceY))
  where
    distanceY = abs (y - targetY)
    distance = manhattenDistance (x, y) (bx, by)
