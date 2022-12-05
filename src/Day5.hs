module Day5 where

import Data.List.Split (divvy, splitOn, splitWhen)
import qualified Data.Map as M

type Stack = [String]
type StackNumber = Int 
type Ship = M.Map StackNumber Stack
data Move = Move {numberOfCrates :: Int, fromStack :: Int, toStack :: Int} deriving (Show, Eq)

solve :: [String] -> String
solve xs = topOfEachStack . foldl performMove ship $ moves
  where
    ship = shipFrom . head . splitWhen (== "") $ xs
    moves = movesFrom . last . splitWhen (== "") $ xs

solvePartTwo :: [String] -> String
solvePartTwo xs = topOfEachStack . foldl performMove2 ship $ moves
  where
    ship = shipFrom . head . splitWhen (== "") $ xs
    moves = movesFrom . last . splitWhen (== "") $ xs

shipFrom :: [String] -> Ship
shipFrom = M.fromList . zip [1 ..] . map (filter (/= " ")) . group . map (divvy 1 4 . tail)
  where
    group [] = []
    group xs
      | all null xs = []
      | otherwise = map head xs : (group . map tail $ xs)

movesFrom :: [String] -> [Move]
movesFrom = map move

move :: String -> Move
move xs =
  Move
    { numberOfCrates = read . (!! 1) . splitOn " " $ xs,
      fromStack = read . (!! 3) . splitOn " " $ xs,
      toStack = read . (!! 5) . splitOn " " $ xs
    }

performMove :: Ship -> Move -> Ship
performMove ship Move {numberOfCrates = n, fromStack = f, toStack = t} = targetShip
  where
    (sourceCrate, sourceShip) = case M.lookup f ship of
      Just a -> (take n a, M.insert f (drop n a) ship)
      Nothing -> ([], ship)

    targetShip = case M.lookup t sourceShip of
      Just a -> M.insert t (reverse sourceCrate ++ a) sourceShip
      Nothing -> ship

performMove2 :: Ship -> Move -> Ship
performMove2 ship Move {numberOfCrates = n, fromStack = f, toStack = t} = targetShip
  where
    (sourceCrate, sourceShip) = case M.lookup f ship of
      Just a -> (take n a, M.insert f (drop n a) ship)
      Nothing -> ([], ship)

    targetShip = case M.lookup t sourceShip of
      Just a -> M.insert t (sourceCrate ++ a) sourceShip
      Nothing -> ship

topOfEachStack :: Ship -> String
topOfEachStack = concatMap (head . snd) . M.toList
