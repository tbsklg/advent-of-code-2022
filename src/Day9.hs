module Day9 where

import Data.List.Split (splitOn)
import qualified Data.Set as S

data Direction = U | D | R | L deriving (Show, Eq, Enum)

data Motion = Motion Direction Int deriving (Show, Eq)

type Position = (Int, Int)

type State = (Position, [Position])

solve :: [String] -> Int
solve = length . visitedOnce . snd . go ((0,0), []) . convertToMotions
  where
    go state [] = state
    go state (x:xs) = go (move state x) xs

convertToMotions :: [String] -> [Motion]
convertToMotions [] = []
convertToMotions (x : xs) = go . splitOn " " $ x
  where
    go ["U", step] = Motion U (read step) : convertToMotions xs
    go ["D", step] = Motion D (read step) : convertToMotions xs
    go ["L", step] = Motion L (read step) : convertToMotions xs
    go ["R", step] = Motion R (read step) : convertToMotions xs
    go _ = error "Could not convert to motion"

move :: State -> Motion -> State
move state (Motion _ 0) = state
move ((hx, hy), tailPositions) (Motion R step)
  | null tailPositions = move (newHead, newTail : tailPositions) (Motion R (step -1))
  | isConnected newHead (head tailPositions) = move (newHead, tailPositions) (Motion R (step -1))
  | otherwise = move (newHead, newTail : tailPositions) (Motion R (step -1))
  where
    newHead@(newHeadX, newHeadY) = (hx + 1, hy)
    newTail = (newHeadX - 1, newHeadY)
move ((hx, hy), tailPositions) (Motion D step)
  | null tailPositions = move (newHead, newTail : tailPositions) (Motion D (step -1))
  | isConnected newHead (head tailPositions) = move (newHead, tailPositions) (Motion D (step -1))
  | otherwise = move (newHead, newTail : tailPositions) (Motion D (step -1))
  where
    newHead@(newHeadX, newHeadY) = (hx, hy - 1)
    newTail = (newHeadX, newHeadY + 1)
move ((hx, hy), tailPositions) (Motion L step)
  | null tailPositions = move (newHead, newTail : tailPositions) (Motion L (step -1))
  | isConnected newHead (head tailPositions) = move (newHead, tailPositions) (Motion L (step -1))
  | otherwise = move (newHead, newTail : tailPositions) (Motion L (step -1))
  where
    newHead@(newHeadX, newHeadY) = (hx - 1, hy)
    newTail = (newHeadX + 1, newHeadY)
move ((hx, hy), tailPositions) (Motion U step)
  | null tailPositions = move (newHead, newTail : tailPositions) (Motion U (step -1))
  | isConnected newHead (head tailPositions) = move (newHead, tailPositions) (Motion U (step -1))
  | otherwise = move (newHead, newTail : tailPositions) (Motion U (step -1))
  where
    newHead@(newHeadX, newHeadY) = (hx, hy + 1)
    newTail = (newHeadX, newHeadY - 1)

isConnected :: (Int, Int) -> (Int, Int) -> Bool
isConnected (hx, hy) (tx, ty) = overlays || directly || diagonally
  where
    overlays = hx == tx && hy == ty
    directly = abs (hx - tx) == 1 && hy == ty || abs (hy - ty) == 1 && hx == tx
    diagonally = abs (hx - tx) == 1 && abs (hy - ty) == 1

visitedOnce :: [(Int, Int)] -> [(Int, Int)]
visitedOnce = S.toList . S.fromList
