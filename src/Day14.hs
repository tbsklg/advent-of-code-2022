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

parse :: [String] -> [Point]
parse [] = []
parse (r : rs) = (concat . go . map extractPoint . splitOn " -> " $ r) ++ parse rs
  where
    go [] = []
    go [_] = []
    go (x : y : ys) = flatten x y : go (y : ys)

extractPoint :: String -> Point
extractPoint xs = (read . head $ parts, read . last $ parts)
  where
    parts = splitOn "," xs

normalize :: [Point] -> Point -> ([Point], Point)
normalize points (sx, sy) = (normalizedPoints, normalizedPouringPosition)
  where
    normalizedPoints = map normalized points
    normalizedPouringPosition = (sx - minX, sy)

    normalized (x, y) = (x - minX, y)

    minX = minimum . map fst $ points
    minY = minimum . map snd $ points

air :: Char
air = '.'

sand :: (Int, Int)
sand = (500, 0)

grid :: Point -> [Point] -> (Point, Grid)
grid pouringPosition paths = (normalizedPouringPosition, buildRocks normalizedPoints initialGrid)
  where
    buildRocks [] grid = grid
    buildRocks (p : ps) grid = buildRocks ps (replace p '#' grid)

    (normalizedPoints, normalizedPouringPosition) = normalize paths pouringPosition
    initialGrid = emptyGridFrom normalizedPoints

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

flatten :: Point -> Point -> [Point]
flatten from = nub . go from
  where
    go (x, y) (x', y')
      | x == x' = map (\z -> (x, z)) [minimum [y, y'] .. maximum [y, y']]
      | otherwise = map (\z -> (z, y)) [minimum [x, x'] .. maximum [x, x']]

emptyGridFrom :: [Point] -> Grid
emptyGridFrom points = foldl (\y x -> y ++ [replicate xSize air]) [] [0 .. ySize - 1]
  where
    xSize = (+ 1) . maximum . map fst $ points
    ySize = (+ 1) . maximum . map snd $ points
