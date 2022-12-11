module Day10 where

import Data.List.Split (splitOn)

data Instruction = NOOP | ADDX Int deriving (Show, Eq)

data Register = Register Int (Int, Int, Int) deriving (Show, Eq)

solve :: [String] -> Int
solve = signalStrength . signals . execute . parse

parse :: [String] -> [Instruction]
parse [] = []
parse (x : xs)
  | x == "noop" = NOOP : parse xs
  | otherwise = ADDX (read . last . splitOn " " $ x) : parse xs

signalStrength :: [Register] -> Int
signalStrength = foldl (\y (Register cycle (_,value, _)) -> y + (cycle * value)) 0

signals :: [Register] -> [Register]
signals = filter (\(Register cycle (_,_,_)) -> cycle `elem`[20, 60, 100, 140, 180, 220])

execute :: [Instruction] -> [Register]
execute instructions = go instructions (Register 0 (0, 1, 1))
  where
    go [] register = []
    go (NOOP : xs) (Register cycle (_, _, out)) = register : go xs register
      where
        register = Register (cycle + 1) (out, out, out)
    go (ADDX value : xs) (Register cycle (_, _, out)) = firstStep : secondStep : go xs secondStep
      where
        firstStep@(Register cycleOne (inF, execF, outF)) = Register (cycle + 1) (out, out, out)
        secondStep = Register (cycleOne + 1) (outF, execF, value + outF)
