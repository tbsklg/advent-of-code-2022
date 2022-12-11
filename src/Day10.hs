module Day10 where

import Data.List.Split (splitOn, chunksOf)
import Data.List (intercalate)

data Instruction = NOOP | ADDX Int deriving (Show, Eq)

data Register = Register Int (Int, Int) deriving (Show, Eq)

solve :: [String] -> Int
solve = signalStrength . signals . execute . parse

solvePartTwo :: [String] -> String
solvePartTwo = screen . draw . execute . parse

parse :: [String] -> [Instruction]
parse [] = []
parse (x : xs)
  | x == "noop" = NOOP : parse xs
  | otherwise = ADDX (read . last . splitOn " " $ x) : parse xs

screen :: String -> String
screen = intercalate "\n" . chunksOf 40

draw :: [Register] -> String
draw [] = ""
draw (Register cycle (incoming, outgoing):rs)
    | (cycle - 1) `mod` 40 `elem` sprite = "#" ++ draw rs
    | otherwise = "." ++ draw rs
    where
        sprite = [incoming - 1, incoming, incoming + 1]

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
