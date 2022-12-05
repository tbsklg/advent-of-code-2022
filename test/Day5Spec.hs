module Day5Spec where

import Day5
import Day5 (Move (numberOfCrates))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "supply stacks" $ do
    it "should convert crates from raw data" $ do
      crates ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"] `shouldBe` [[" ", "N", "Z"], ["D", "C", "M"], [" ", " ", "P"]]

    it "should convert moves from raw data" $ do
      moves ["move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"]
        `shouldBe` [
            (Move {numberOfCrates = 1, from = 2, to = 1}),
            (Move {numberOfCrates = 3, from = 1, to = 3}),
            (Move {numberOfCrates = 2, from = 2, to = 1}),
            (Move {numberOfCrates = 1, from = 1, to = 2})
        ]
