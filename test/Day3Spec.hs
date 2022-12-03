module Day3Spec where

import Day3
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "rucksack reorganization" $ do
    it "should calculate the priority of items which appear in both compartments of a rucksack" $ do
      solve ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
        `shouldBe` 157

    it "should split a rucksack into two compartments" $ do
      compartmentsOfA "vJrwpWtwJgWrhcsFMMfFFhFp" `shouldBe` ("vJrwpWtwJgWr", "hcsFMMfFFhFp")

    it "should find the item which appears in both compartments" $ do
      itemThatAppearsInBoth ("vJrwpWtwJgWr", "hcsFMMfFFhFp") `shouldBe` 'p'

    it "should calculate the priority of one item" $ do
      priorityOfA 'p' `shouldBe` 16
      priorityOfA 'L' `shouldBe` 38

    it "should group all rucksack to a size of three" $ do
      groupOfThree ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
        `shouldBe` [["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"], ["wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]]

      groupOfThree
        ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT"]
        `shouldBe` [["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"]]

    it "should find the batch that appears in a group of three" $ do
      batchThatAppearsInAGroupOfThree ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg"] `shouldBe` 'r'

    it "should calculate the priority of batches that appear in a group of three elves" $ do
      solvePartTwo ["vJrwpWtwJgWrhcsFMMfFFhFp", "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL", "PmmdzqPrVvPwwTWBwg", "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn", "ttgJtRGJQctTZtZT", "CrZsJsPPZsGzwwsLwLmpwMDw"]
        `shouldBe` 70