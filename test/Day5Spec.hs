module Day5Spec where

import qualified Data.Map as M
import Day5
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "supply stacks" $ do
    it "should convert crates from raw data" $ do
      shipFrom ["    [D]    ", "[N] [C]    ", "[Z] [M] [P]"] `shouldBe` M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]

    it "should convert moves from raw data" $ do
      movesFrom ["move 1 from 2 to 1", "move 3 from 1 to 3", "move 2 from 2 to 1", "move 1 from 1 to 2"]
        `shouldBe` [ (Move {numberOfCrates = 1, fromStack = 2, toStack = 1}),
                     (Move {numberOfCrates = 3, fromStack = 1, toStack = 3}),
                     (Move {numberOfCrates = 2, fromStack = 2, toStack = 1}),
                     (Move {numberOfCrates = 1, fromStack = 1, toStack = 2})
                   ]

    it "should perform a move with Cargo Crane" $ do
      performMove withCargoCrane (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 1, fromStack = 2, toStack = 1}) `shouldBe` M.fromList [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
      performMove withCargoCrane (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 2, fromStack = 2, toStack = 1}) `shouldBe` M.fromList [(1, ["C", "D", "N", "Z"]), (2, ["M"]), (3, ["P"])]

    it "should perform a move with Crate Mover 9000" $ do
      performMove withCrateMover9000 (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 1, fromStack = 2, toStack = 1}) `shouldBe` M.fromList [(1, ["D", "N", "Z"]), (2, ["C", "M"]), (3, ["P"])]
      performMove withCrateMover9000 (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) (Move {numberOfCrates = 2, fromStack = 2, toStack = 1}) `shouldBe` M.fromList [(1, ["D", "C", "N", "Z"]), (2, ["M"]), (3, ["P"])]

    it "should caputure the first crate from each stack" $ do
      topOfEachStack (M.fromList [(1, ["N", "Z"]), (2, ["D", "C", "M"]), (3, ["P"])]) `shouldBe` "NDP"
