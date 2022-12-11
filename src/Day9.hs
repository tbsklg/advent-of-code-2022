module Day9 where

import Data.List (nub)
import Data.List.Split (splitOn)

data Direction = U | D | R | L deriving (Show, Eq, Enum)

type Position = (Int, Int)

data Motion = Motion Direction Int deriving (Show, Eq)

type State = [Position]

solve :: [String] -> Int
solve = length . nub . go [(0, 0)] . convertToMotions
  where
    go state [] = state
    go state (x : xs) = go (performMotion state x) xs

solvePartTwo :: [String] -> Int
solvePartTwo =
  length . nub . map last
    . scanl performMotionWithNKnots (replicate 10 (0, 0))
    . flatten
    . convertToMotions

flatten :: [Motion] -> [Direction]
flatten [] = []
flatten (Motion direction step : xs) = replicate step direction ++ flatten xs

convertToMotions :: [String] -> [Motion]
convertToMotions [] = []
convertToMotions (x : xs) = go . splitOn " " $ x
  where
    go ["U", step] = Motion U (read step) : convertToMotions xs
    go ["D", step] = Motion D (read step) : convertToMotions xs
    go ["L", step] = Motion L (read step) : convertToMotions xs
    go ["R", step] = Motion R (read step) : convertToMotions xs
    go _ = error "Could not convert to motion"

performMotionWithNKnots :: [Position] -> Direction -> [Position]
performMotionWithNKnots knots direction = go [] knots
  where
    go newKnots [] = newKnots
    go [] (h : ks) = go [move h direction] ks
    go robe (k : ks) = go (robe ++ [moveTo (last robe) k]) ks

performMotion :: State -> Motion -> State
performMotion [] _ = []
performMotion state (Motion _ 0) = state
performMotion rope@[h] (Motion direction steps) = performMotion (move h direction : rope) (Motion direction (steps - 1))
performMotion rope@(h : ks) (Motion direction steps) = performMotion nextKs (Motion direction (steps - 1))
  where
    nextH = move h direction
    nextT = moveTo nextH (head ks)
    nextKs = nextH : (nextT : ks)

move :: Position -> Direction -> Position
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)
move (x, y) U = (x, y + 1)
move (x, y) D = (x, y - 1)

moveTo :: (Int, Int) -> (Int, Int) -> (Int, Int)
moveTo (hx, hy) (tx, ty)
  | abs (hx - tx) <= 1 && abs (hy - ty) <= 1 = (tx, ty)
  | otherwise = (tx + signum (hx - tx), ty + signum (hy - ty))
