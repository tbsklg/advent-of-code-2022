module Day10 where

import Data.List.Split (splitOn)

data Instruction = NOOP | ADDX Int deriving (Show, Eq)

data Register = Register Int (Int, Int) deriving (Show, Eq)

solve :: [String] -> Int
solve = signalStrength . signals . execute . parse

registers :: [String] -> [Register]
registers = execute . parse

parse :: [String] -> [Instruction]
parse [] = []
parse (x : xs)
  | x == "noop" = NOOP : parse xs
  | otherwise = ADDX (read . last . splitOn " " $ x) : parse xs

draw :: [Register] -> [Int] -> String
draw [] sprite = ""
draw (Register cycle (incoming, outgoing):rs) sprite
    | cycle `elem` sprite = "#" ++ draw rs nextSprite
    | otherwise = "." ++ draw rs nextSprite
    where
        nextSprite = [outgoing - 1, outgoing, outgoing + 1]

signalStrength :: [Register] -> Int
signalStrength = foldl (\y (Register cycle (value, _)) -> y + (cycle * value)) 0

signals :: [Register] -> [Register]
signals = filter (\(Register cycle (_,_)) -> cycle `elem`[20, 60, 100, 140, 180, 220])

execute :: [Instruction] -> [Register]
execute instructions = go instructions (Register 0 (1, 1))
  where
    go [] register = []
    go (NOOP : xs) (Register cycle ( _, finish)) = register : go xs register
      where
        register = Register (cycle + 1) (finish, finish)
    go (ADDX value : xs) (Register cycle (_, finish)) = firstStep : secondStep : go xs secondStep
      where
        firstStep@(Register cycleOne (inF, outF)) = Register (cycle + 1) (finish, finish)
        secondStep = Register (cycleOne + 1) (inF, value + outF)
