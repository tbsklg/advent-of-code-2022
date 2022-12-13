module Day12 where

import Data.List (elemIndex, elemIndices, nub, sort)
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Set as S
import Debug.Trace

type Location = (Int, Int)

type Grid = [String]

data BFS = BFS
  { queue :: [Location],
    visited :: S.Set Location,
    parents :: M.Map Location Location,
    validElevation :: Char -> Char -> Bool
  }

solve :: Grid -> Int
solve = length . performBFS initialBFS 'E'
  where
    startLocation = (0, 0)
    initialQueue = [startLocation]
    initialVisited = S.empty
    initialParents = M.empty

    initialBFS = (BFS {queue = initialQueue, visited = initialVisited, parents = initialParents, validElevation = isValidElevationForwards})

solvePartTwo :: Grid -> Int
solvePartTwo grid = length . performBFS initialBFS 'a' $ grid
  where
    startLocation = findEnd grid
    initialQueue = [startLocation]
    initialVisited = S.empty
    initialParents = M.empty

    initialBFS = (BFS {queue = initialQueue, visited = initialVisited, parents = initialParents, validElevation = isValidElevationBackwards})

findEnd :: Grid -> Location
findEnd = find . zip [0 ..]
  where
    find [] = error "Target not found"
    find ((row, elems) : xs) = case elemIndex 'E' elems of
      Just a -> (row, a)
      Nothing -> find xs

performBFS :: BFS -> Char -> Grid -> [Location]
performBFS current@BFS {queue = queue, visited = visited, parents = parents, validElevation = validElevation} target grid
  | null queue = []
  | S.member top visited = performBFS (current {queue = rest}) target grid
  | elevation grid top == target = buildPathFrom parents [top]
  | otherwise = performBFS (current {queue = nextQueue, visited = nextVisited, parents = nextParents}) target grid
  where
    (top : rest) = queue
    neighbours = adjacentNeighbours visited top grid validElevation

    nextParents = foldl (\y x -> M.insert x top y) parents neighbours

    nextQueue = rest ++ neighbours

    nextVisited = S.insert top visited

adjacentNeighbours :: S.Set Location -> Location -> Grid -> (Char -> Char -> Bool) -> [Location]
adjacentNeighbours visited top grid validElevation =
  filter (\x -> not $ S.member x visited)
    . map fst
    . filter (\(_, y) -> validElevation currentElevation y)
    . map (\x -> (x, elevation grid x))
    . neighboursOf top
    $ grid
  where
    currentElevation = elevation grid top

buildPathFrom :: M.Map Location Location -> [Location] -> [Location]
buildPathFrom parents target = case M.lookup (head target) parents of
  Nothing -> tail target
  Just a -> buildPathFrom parents (a : target)

elevations :: [Char]
elevations = ['a' .. 'z']

isValidElevationBackwards :: Char -> Char -> Bool
isValidElevationBackwards from to
  | from == 'E' = True
  | to == 'E' = False
  | to == from = True
  | otherwise = (fromJust . elemIndex from $ elevations) - (fromJust . elemIndex to $ elevations) <= 1

isValidElevationForwards :: Char -> Char -> Bool
isValidElevationForwards from to
  | from == 'S' = True
  | from == 'z' && to == 'E' = True
  | to == 'E' = False
  | to == 'S' = False
  | to == from = True
  | otherwise = (fromJust . elemIndex to $ elevations) - (fromJust . elemIndex from $ elevations) <= 1

elevation :: Grid -> Location -> Char
elevation xs (x, y) = xs !! x !! y

neighboursOf :: Location -> Grid -> [Location]
neighboursOf (x, y) xs = catMaybes [left, right, up, down]
  where
    left = if (y - 1) < 0 then Nothing else Just (x, y - 1)
    right = if (y + 1) > (length (head xs) -1) then Nothing else Just (x, y + 1)
    up = if (x - 1) < 0 then Nothing else Just (x - 1, y)
    down = if (x + 1) > (length xs - 1) then Nothing else Just (x + 1, y)
