module Day3 where
import Data.List ( elemIndex, intersect )
import Data.List.Split (divvy)

type Rucksacks = [String]
type Rucksack = String
type Items = String
type Item = Char
type Compartment = (Items, Items)
type Priorities = String
type Priority = Int

solve :: Rucksacks -> Int
solve = sum . map ((priorityOfA . itemThatAppearsInBoth) . compartmentsOfA)

solvePartTwo :: Rucksacks -> Int
solvePartTwo = sum . map (priorityOfA . batchThatAppearsInAGroupOfThree) . groupOfThree

compartmentsOfA :: Rucksack -> Compartment
compartmentsOfA xs = splitAt half xs
    where
        half = length xs `div` 2

itemThatAppearsInBoth :: Compartment -> Item
itemThatAppearsInBoth (first, second) = head $ first `intersect` second

priorities :: Priorities
priorities = ['a' .. 'z'] ++ ['A' .. 'Z']

priorityOfA :: Item -> Priority
priorityOfA item = case elemIndex item priorities of
    Nothing -> 0
    Just a -> a + 1

batchThatAppearsInAGroupOfThree :: Rucksacks -> Char
batchThatAppearsInAGroupOfThree [first, second, third] = head . intersect first . intersect second $ third
batchThatAppearsInAGroupOfThree _ = error "No group of three provided!"

groupOfThree :: Rucksacks -> [Rucksacks]
groupOfThree = divvy 3 3
