{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day14 where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Data.List.Split (splitOn)
import Debug.Trace (traceShow)

type Point = (Int, Int)

type Grid = [String]

data Path = Path {from :: Point, to :: Point} deriving (Show, Eq)

solve :: [String] -> Int
solve = length . filter (=='o') . concat . furtherFallings . grid (500,0) . parse

parse :: [String] -> [Path]
parse [] = []
parse (r : rs) = extractedPath ++ parse rs
  where
    extractedPath = go . map extractPoint . splitOn " -> " $ r

    go [] = []
    go [_] = []
    go (x : y : ys) = (Path {from = x, to = y}) : go (y : ys)

extractPoint :: String -> Point
extractPoint xs = (read . head $ parts, read . last $ parts)
  where
    parts = splitOn "," xs

normalize :: [Path] -> Point -> ([Path], Point)
normalize paths (sx, sy) = (normalizedPath, normalizedPouringPosition)
  where
    normalizedPath = map (\x -> Path {from = normalized . from $ x, to = normalized . to $ x}) paths
    normalizedPouringPosition = (sx - minX, sy)

    normalized (x, y) = (x - minX, y)

    minX = minimum . foldl (\y x -> fst (from x) : fst (to x) : y) [] $ paths
    minY = minimum . foldl (\y x -> snd (from x) : snd (to x) : y) [] $ paths

air :: Char
air = '.'

sand :: (Int, Int)
sand = (500, 0)

grid :: Point -> [Path] -> (Point, Grid)
grid pouringPosition paths = (normalizedPouringPosition, buildRocks (flatten normalizedPath) initialGrid)
  where
    buildRocks [] grid = grid
    buildRocks (p : ps) grid = buildRocks ps (replace p '#' grid)

    (normalizedPath, normalizedPouringPosition) = normalize paths pouringPosition
    initialGrid = emptyGridFrom normalizedPath

furtherFallings :: (Point, Grid) -> Grid
furtherFallings (pouringPosition, abc) = fallDownFrom pouringPosition abc
  where
    fallDownFrom (x, y) grid = case fallDown (x,y) grid of
      Just a -> fallDownFrom pouringPosition (replace a 'o' grid)
      Nothing -> grid

fallDown :: Point -> Grid -> Maybe Point
fallDown _ [] = Nothing
fallDown point@(x, y) grid
  | outOfBounds point grid = Nothing
  | blocked point grid = Nothing
  | not . blocked (x, y + 1) $ grid = fallDown (x, y + 1) grid
  | not . blocked (x - 1, y + 1) $ grid = fallDown (x - 1, y + 1) grid
  | not . blocked (x + 1, y + 1) $ grid = fallDown (x + 1, y + 1) grid
  | otherwise = Just (x, y)

blocked :: Point -> Grid -> Bool
blocked (x, y) grid = (grid !! y !! x) `elem` "#o"

outOfBounds :: Point -> Grid -> Bool
outOfBounds _ [] = True
outOfBounds (x, y) grid@(g : gs) = x == 0

replace :: Point -> Char -> Grid -> Grid
replace (x, y) toInsert grid = nextGrid
  where
    lineToUpdate = (!! max 0 y) grid
    updatedLine = take x lineToUpdate ++ [toInsert] ++ drop (x + 1) lineToUpdate
    nextGrid = take y grid ++ [updatedLine] ++ drop (y + 1) grid

flatten :: [Path] -> [(Int, Int)]
flatten = nub . go
  where
    go [] = []
    go (Path {from = (x, y), to = (x', y')} : ps)
      | x == x' = map (\z -> (x, z)) [minimum [y, y'] .. maximum [y, y']] ++ go ps
      | otherwise = map (\z -> (z, y)) [minimum [x, x'] .. maximum [x, x']] ++ go ps

emptyGridFrom :: [Path] -> Grid
emptyGridFrom paths = foldl (\y x -> y ++ [replicate xSize air]) [] [0 .. ySize - 1]
  where
    xSize = (+ 1) . maximum . foldl (\y x -> fst (from x) : fst (to x) : y) [] $ paths
    ySize = (+ 1) . maximum . foldl (\y x -> snd (from x) : snd (to x) : y) [] $ paths
