module Day2 where

import Data.List.Split (splitOn)
import Day1 (solvePartTwo)

type Game = (RPS, RPS)

data RPS = Rock | Paper | Scissor deriving (Eq, Show)

data Result = Win | Loss | Draw deriving (Eq, Show)

solve :: [String] -> Int
solve = foldl playSimpleGame 0 . map convert

solvePartTwo :: [String] -> Int
solvePartTwo = foldl playGameWithStrategy 0 . map convertPartTwo

playSimpleGame :: Int -> (RPS, RPS) -> Int
playSimpleGame endResult round@(opponentRPS, myRPS) = endResult + pointsPerGame
  where
    pointsPerGame = pointsForRPS myRPS + (pointsForResult . resultForGame $ round)

playGameWithStrategy :: Int -> (RPS, Result) -> Int
playGameWithStrategy endResult round@(opponentRPS, desiredResult) = endResult + pointsPerGame
  where
    pointsPerGame = pointsForResult desiredResult + (pointsForRPS . rpsForDesiredResult $ round)

convert :: String -> Game
convert raw = (toRPS . head $ raw, toRPS . last $ raw)

convertPartTwo :: String -> (RPS, Result)
convertPartTwo raw = (toRPS . head $ raw, convertToResult . last $ raw)

toRPS :: Char -> RPS
toRPS 'A' = Rock
toRPS 'X' = Rock
toRPS 'B' = Paper
toRPS 'Y' = Paper
toRPS 'C' = Scissor
toRPS 'Z' = Scissor
toRPS _ = error "Could not convert to `RPS`!"

convertToResult :: Char -> Result
convertToResult 'X' = Loss
convertToResult 'Y' = Draw
convertToResult 'Z' = Win
convertToResult _ = error "Could not convert to `Result`!"

pointsForResult :: Result -> Int
pointsForResult Win = 6
pointsForResult Draw = 3
pointsForResult Loss = 0

pointsForRPS :: RPS -> Int
pointsForRPS Rock = 1
pointsForRPS Paper = 2
pointsForRPS Scissor = 3

resultForGame :: Game -> Result
resultForGame (Rock, Rock) = Draw
resultForGame (Rock, Paper) = Win
resultForGame (Rock, Scissor) = Loss
resultForGame (Paper, Rock) = Loss
resultForGame (Paper, Paper) = Draw
resultForGame (Paper, Scissor) = Win
resultForGame (Scissor, Rock) = Win
resultForGame (Scissor, Paper) = Loss
resultForGame (Scissor, Scissor) = Draw

rpsForDesiredResult :: (RPS, Result) -> RPS
rpsForDesiredResult (Rock, Loss) = Scissor
rpsForDesiredResult (Rock, Draw) = Rock
rpsForDesiredResult (Rock, Win) = Paper
rpsForDesiredResult (Paper, Loss) = Rock
rpsForDesiredResult (Paper, Draw) = Paper
rpsForDesiredResult (Paper, Win) = Scissor
rpsForDesiredResult (Scissor, Loss) = Paper
rpsForDesiredResult (Scissor, Draw) = Scissor
rpsForDesiredResult (Scissor, Win) = Rock
