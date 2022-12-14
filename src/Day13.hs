module Day13 where

import Data.Char (isDigit, isNumber)
import Data.List (elemIndex, sortBy)
import Data.Maybe (fromJust)

type Signal = [String]

type Package = String

solve :: Signal -> Integer
solve = sum . map fst . filter snd . zipWith (curry (\(index, (firstSignal, secondSignal)) -> (index, compareSignal firstSignal secondSignal))) [1 ..] . parse

solvePartTwo :: Signal -> Int
solvePartTwo xs = firstLocation * secondLocation
  where
    firstLocation = (+) 1 . fromJust . elemIndex (head dividerPackets) $ sortedPackets
    secondLocation = (+) 1 . fromJust . elemIndex (last dividerPackets) $ sortedPackets
    sortedPackets = sortBy sortSignals . (++) dividerPackets . filter (/= "") $ xs

dividerPackets :: [Package]
dividerPackets = ["[[2]]", "[[6]]"]

parse :: Signal -> [(Package, Package)]
parse raw
  | null currentChunk = []
  | otherwise = (head currentChunk, last currentChunk) : parse next
  where
    currentChunk = takeWhile (/= "") raw

    nextChunk = dropWhile (/= "") raw
    next
      | null nextChunk = []
      | otherwise = tail nextChunk

sortSignals :: Package -> Package -> Ordering
sortSignals ('[' : xs) ('[' : ys) = sortSignals xs ys
sortSignals (']' : xs) (']' : ys) = sortSignals xs ys
sortSignals (',' : xs) (',' : ys) = sortSignals xs ys
sortSignals a@(']' : _) _ = LT
sortSignals _ (']' : _) = GT
sortSignals a@('[' : xs) b@(y : ys) = sortSignals a (convertToList b)
sortSignals a@(x : xs) b@('[' : ys) = sortSignals (convertToList a) b
sortSignals a@(x : xs) b@(y : ys) =
  case compare numberA numberB of
    LT -> LT
    GT -> GT
    EQ -> sortSignals furtherA furtherA
  where
    (numberA, furtherA) = popInteger a
    (numberB, furtherB) = popInteger b
sortSignals a b = LT

compareSignal :: Package -> Package -> Bool
compareSignal ('[' : xs) ('[' : ys) = compareSignal xs ys
compareSignal (']' : xs) (']' : ys) = compareSignal xs ys
compareSignal (',' : xs) (',' : ys) = compareSignal xs ys
compareSignal a@(']' : _) _ = True
compareSignal _ (']' : _) = False
compareSignal a@('[' : xs) b@(y : ys) = compareSignal a (convertToList b)
compareSignal a@(x : xs) b@('[' : ys) = compareSignal (convertToList a) b
compareSignal a@(x : xs) b@(y : ys)
  | retrieveDigits && numberA == numberB = compareSignal furtherA furtherB
  | retrieveDigits && numberA < numberB = True
  | otherwise = False
  where
    retrieveDigits = isDigit x && isDigit y

    (numberA, furtherA) = popInteger a
    (numberB, furtherB) = popInteger b
compareSignal a b = False

popInteger :: String -> (Int, String)
popInteger xs = (number, further)
  where
    number = read . takeWhile isDigit $ xs
    further = dropWhile isDigit xs

convertToList :: String -> String
convertToList xs = '[' : show number ++ [']'] ++ xs
  where
    (number, further) = popInteger xs
