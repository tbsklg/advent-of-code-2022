module Day5 where

import Data.List.Split (divvy, splitOn, splitWhen)
import qualified Data.Map as M

data Move = Move {numberOfCrates :: Int, from :: Int, to :: Int} deriving (Show, Eq)

solve :: [String] -> String
solve xs = topOfEachStack . foldl performMove (crates stacks) $ moves rawMoves
  where
    stacks = head . splitWhen (== "") $ xs
    rawMoves = last . splitWhen (== "") $ xs

solvePartTwo :: [String] -> String
solvePartTwo xs = topOfEachStack . foldl performMove2 (crates stacks) $ moves rawMoves
  where
    stacks = head . splitWhen (== "") $ xs
    rawMoves = last . splitWhen (== "") $ xs
  
crates :: [String] -> M.Map Int [String]
crates = M.fromList . zip [1 ..] . map (filter (/= " ")) . group . map (divvy 1 4 . tail)
  where
    group [] = []
    group xs
      | all null xs = []
      | otherwise = map head xs : (group . map tail $ xs)

moves :: [String] -> [Move]
moves = map move

move :: String -> Move
move xs =
  Move
    { numberOfCrates = read . (!! 1) . splitOn " " $ xs,
      from = read . (!! 3) . splitOn " " $ xs,
      to = read . (!! 5) . splitOn " " $ xs
    }

performMove :: M.Map Int [String] -> Move -> M.Map Int [String]
performMove ship Move {numberOfCrates = n, from = f, to = t} = targetShip
  where
    (sourceCrate, sourceShip) = case M.lookup f ship of
      Just a -> (take n a, M.insert f (drop n a) ship)
      Nothing -> ([], ship)

    targetShip = case M.lookup t sourceShip of
      Just a -> M.insert t (reverse sourceCrate ++ a) sourceShip
      Nothing -> ship

performMove2 :: M.Map Int [String] -> Move -> M.Map Int [String]
performMove2 ship Move {numberOfCrates = n, from = f, to = t} = targetShip
  where
    (sourceCrate, sourceShip) = case M.lookup f ship of
      Just a -> (take n a, M.insert f (drop n a) ship)
      Nothing -> ([], ship)

    targetShip = case M.lookup t sourceShip of
      Just a -> M.insert t (sourceCrate ++ a) sourceShip
      Nothing -> ship

topOfEachStack :: M.Map a [String] -> String
topOfEachStack = concatMap (head . snd) . M.toList
