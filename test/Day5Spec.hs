module Day5Spec where

import qualified Data.Map as M
import Day5 (Move (Move, from, numberOfCrates, to), crates, moves, performMove, topOfEachStack)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "supply stacks" $ do
    it "should convert crates from raw data" $ do
      crates ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"] `shouldBe` M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]

    it "should convert moves from raw data" $ do
      moves ["move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"]
        `shouldBe` [ (Move {numberOfCrates = 1, from = 2, to = 1}),
                     (Move {numberOfCrates = 3, from = 1, to = 3}),
                     (Move {numberOfCrates = 2, from = 2, to = 1}),
                     (Move {numberOfCrates = 1, from = 1, to = 2})
                   ]

    it "should perform a move" $ do
      performMove (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 1, from = 2, to = 1}) `shouldBe` M.fromList [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
      performMove (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 2, from = 2, to = 1}) `shouldBe` M.fromList [(1, ["C", "D", "N", "Z"]), (2, ["M"]), (3, ["P"])]

    it "should caputure the first crate from each stack" $ do
      topOfEachStack (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) `shouldBe` "NDP"
    