module Day14 where

import Data.Function (on)
import Data.List (maximumBy, minimumBy, nub)
import Data.List.Split (splitOn)
import qualified Data.Set as S

type Point = (Int, Int)

type Grid = [String]

data Path = Path {from :: Point, to :: Point} deriving (Show, Eq)

solve :: [String] -> Int
solve = simulateFalling . grid (500, 0) . parse

solvePartTwo :: [String] -> Int
solvePartTwo xs = simulateFalling2 . grid (500, 0) $ parse xs

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

air :: Char
air = '.'

sand :: (Int, Int)
sand = (500, 0)

grid :: Point -> [Point] -> (Point, S.Set Point)
grid pouringPosition points = (pouringPosition, buildRocks points initialGrid)
  where
    buildRocks [] grid = grid
    buildRocks (p : ps) grid = buildRocks ps (S.insert p grid)

    initialGrid = S.empty

simulateFalling :: (Point, S.Set Point) -> Int
simulateFalling (pouringPosition, abc) = fallDownFrom pouringPosition abc
  where
    maxY = maximum . map snd . S.toList $ abc

    fallDownFrom (x, y) grid = case fallDown (x, y) maxY grid of
      Just a -> 1 + fallDownFrom pouringPosition (S.insert a grid)
      Nothing -> 0

simulateFalling2 :: (Point, S.Set Point) -> Int
simulateFalling2 (pouringPosition, abc) = fallDownFrom pouringPosition abc
  where
    maxY = (+) 1 . maximum . map snd . S.toList $ abc

    fallDownFrom (x, y) grid = case fallDown2 (x, y) maxY grid of
      Just a -> 1 + fallDownFrom pouringPosition (S.insert a grid)
      Nothing -> 0

fallDown :: Point -> Int -> S.Set Point -> Maybe Point
fallDown point@(x, y) maxY grid
  | blocked point grid = Nothing
  | y == maxY = Nothing
  | not . blocked2 (x, y + 1) maxY $ grid = fallDown (x, y + 1) maxY grid
  | not . blocked2 (x - 1, y + 1) maxY $ grid = fallDown (x - 1, y + 1) maxY grid
  | not . blocked2 (x + 1, y + 1) maxY $ grid = fallDown (x + 1, y + 1) maxY grid
  | otherwise = Just (x, y)

fallDown2 :: Point -> Int -> S.Set Point -> Maybe Point
fallDown2 point@(x, y) maxY grid
  | blocked2 point maxY grid = Nothing
  | not . blocked2 (x, y + 1) maxY $ grid = fallDown2 (x, y + 1) maxY grid
  | not . blocked2 (x - 1, y + 1) maxY $ grid = fallDown2 (x - 1, y + 1) maxY grid
  | not . blocked2 (x + 1, y + 1) maxY $ grid = fallDown2 (x + 1, y + 1) maxY grid
  | otherwise = Just (x, y)

blocked2 :: Point -> Int -> S.Set Point -> Bool
blocked2 point@(x,y) maxY s = S.member point s || y > maxY

blocked :: Point -> S.Set Point -> Bool
blocked = S.member

flatten :: Point -> Point -> [Point]
flatten from = nub . go from
  where
    go (x, y) (x', y')
      | x == x' = map (\z -> (x, z)) [minimum [y, y'] .. maximum [y, y']]
      | otherwise = map (\z -> (z, y)) [minimum [x, x'] .. maximum [x, x']]
