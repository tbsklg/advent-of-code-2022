module Day5 where

import Data.List.Split (divvy, splitOn, splitWhen)
import qualified Data.Map as M

type Stack = [String]

type Crates = [String]

type StackNumber = Int

type Ship = M.Map StackNumber Stack

data Move = Move {numberOfCrates :: Int, fromStack :: Int, toStack :: Int} deriving (Show, Eq)

solve :: [String] -> String
solve xs = topOfEachStack . foldl (performMove withCargoCrane) ship $ moves
  where
    ship = shipFrom . head . splitWhen (== "") $ xs
    moves = movesFrom . last . splitWhen (== "") $ xs

solvePartTwo :: [String] -> String
solvePartTwo xs = topOfEachStack . foldl (performMove withCrateMover9000) ship $ moves
  where
    ship = shipFrom . head . splitWhen (== "") $ xs
    moves = movesFrom . last . splitWhen (== "") $ xs

withCargoCrane :: Crates -> Stack -> Stack
withCargoCrane c s = reverse c ++ s

withCrateMover9000 :: Crates -> Stack -> Stack
withCrateMover9000 c s = c ++ s

shipFrom :: [String] -> Ship
shipFrom = M.fromList . zip [1 ..] . map (filter (/= " ")) . groupByStack . map (divvy 1 4 . tail)
  where
    groupByStack [] = []
    groupByStack xs
      | all null xs = []
      | otherwise = map head xs : (groupByStack . map tail $ xs)

movesFrom :: [String] -> [Move]
movesFrom = map move

move :: String -> Move
move xs =
  Move
    { numberOfCrates = read . (!! 1) . splitOn " " $ xs,
      fromStack = read . (!! 3) . splitOn " " $ xs,
      toStack = read . (!! 5) . splitOn " " $ xs
    }

performMove :: (Crates -> Stack -> Stack) -> Ship -> Move -> Ship
performMove crane ship Move {numberOfCrates = numberOfCrates, fromStack = fromStack, toStack = toStack} = loadedShip
  where
    (unloadedCrate, unloadedShip) = case M.lookup fromStack ship of
      Just stack -> (take numberOfCrates stack, M.insert fromStack (drop numberOfCrates stack) ship)
      Nothing -> ([], ship)

    loadedShip = case M.lookup toStack unloadedShip of
      Just a -> M.insert toStack (crane unloadedCrate a) unloadedShip
      Nothing -> ship

topOfEachStack :: Ship -> String
topOfEachStack = concatMap (head . snd) . M.toList
