module Day11 where

import Data.Char (isNumber)
import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import GHC.OldList (nub)

data Note = Note
  { items :: [Integer],
    operationFn :: Integer -> Integer,
    recipientFn :: Integer -> Integer,
    interactionCount :: Integer,
    divisor :: Integer
  }

type Monkeys = M.Map Integer Note

solve :: [String] -> Integer
solve = playN 20

solvePartTwo :: [String] -> Integer
solvePartTwo = playN 10000

lcmFor :: [Integer] -> Integer
lcmFor = foldr lcm 1

playN :: Integer -> [String] -> Integer
playN n raw = 
  product
    . take 2
    . reverse
    . sort
    . map (interactionCount . snd)
    . M.toList
    . playRounds n lcm
    $ monkeys
  where
    lcm = lcmFor . map (divisor . snd) . M.toList $ monkeys
    monkeys = parse raw

playRounds :: Integer -> Integer -> Monkeys -> Monkeys
playRounds 0 _ monkeys = monkeys
playRounds i lcm monkeys = playRounds (i - 1) lcm (play lcm monkeys)

play :: Integer -> Monkeys -> Monkeys
play lcm monkeys = foldl throw monkeys monkeyNumbers
  where
    throw currentMonkeys number = case M.lookup number currentMonkeys of
      Just throwingMonkey -> throwItems (number, lcm, throwingMonkey) currentMonkeys
      Nothing -> error "Could not find monkey to throw to"

    monkeyNumbers = M.keys monkeys

throwItems :: (Integer, Integer, Note) -> Monkeys -> Monkeys
throwItems (_, _, Note {items = []}) monkeys = monkeys
throwItems
  ( number,
    lcm,
    Note
      { items = (i : is),
        operationFn = currentOperationFn,
        recipientFn = currentRecipientFn,
        interactionCount = currentCount,
        divisor = currentDivisor
      }
    )
  monkeys = throwItems (number, lcm, updatedMonkey) nextMonkeys
    where
      recipient = currentRecipientFn worryLevel
      worryLevel = currentOperationFn i `mod` lcm

      updatedMonkey =
        ( Note
            { items = is,
              operationFn = currentOperationFn,
              recipientFn = currentRecipientFn,
              interactionCount = currentCount + 1,
              divisor = currentDivisor
            }
        )
      updatedMonkeys = M.insert number updatedMonkey monkeys

      nextMonkeys = case M.lookup recipient updatedMonkeys of
        Just a ->
          M.insert
            recipient
            ( Note
                { items = items a ++ [worryLevel],
                  operationFn = operationFn a,
                  recipientFn = recipientFn a,
                  interactionCount = interactionCount a,
                  divisor = divisor a
                }
            )
            updatedMonkeys
        Nothing -> error "Could not find a recipient"

parse :: [String] -> Monkeys
parse xs = go xs M.empty
  where
    go notes monkeys
      | null notes = monkeys
      | otherwise = go nextNote nextMonkeys
      where
        nextMonkeys =
          M.insert
            number
            ( Note
                { items = items,
                  operationFn = operation,
                  recipientFn = throwTo,
                  interactionCount = 0,
                  divisor = divisor
                }
            )
            monkeys

        (currentNote, nextNote) = parseNote notes

        number = parseNumberFrom . head $ currentNote
        items = parseItemsFrom . (!! 1) $ currentNote
        operation = parseOperationFn . (!! 2) $ currentNote
        throwTo = parseRecipientFn . drop 3 $ currentNote
        divisor = parseDivisor . drop 3 $ currentNote

parseNote :: [String] -> ([String], [String])
parseNote notes = (takeWhile (/= "") notes, nextNote)
  where
    maybeNextNote = dropWhile (/= "") notes
    nextNote
      | null maybeNextNote = []
      | otherwise = tail maybeNextNote

parseNumberFrom :: String -> Integer
parseNumberFrom = read . last . splitOn " " . init

parseItemsFrom :: String -> [Integer]
parseItemsFrom = map read . splitOn ", " . dropWhile (not . isNumber)

parseOperationFn :: String -> (Integer -> Integer)
parseOperationFn = go . splitOn " "
  where
    go raw
      | operator == "*" && last raw == "old" = \x -> x * x
      | operator == "*" = (* operant)
      | operator == "-" = \x -> x - operant
      | operator == "/" = (`div` operant)
      | operator == "+" = (+ operant)
      | otherwise = error "Could not parse operator"
      where
        operant = read . last $ raw
        operator = last . init $ raw

parseRecipientFn :: [String] -> (Integer -> Integer)
parseRecipientFn xs = \x -> if x `mod` divisor == 0 then true else false
  where
    true = read . last . splitOn " " . (!! 1) $ xs
    false = read . last . splitOn " " . (!! 2) $ xs
    divisor = parseDivisor xs

parseDivisor :: [String] -> Integer
parseDivisor = read . last . splitOn " " . head
