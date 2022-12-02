module Day2Spec where

import Day2 (solve, solvePartTwo)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "rock paper scissors" $ do
    it "should calculate the score when playing with a strategy guide" $ do
      solve ["A Y", "B X", "C Z"] `shouldBe` 15

    it "should calculate the score when playing with a ultra secret strategy guide" $ do
      solvePartTwo ["A Y", "B X", "C Z"] `shouldBe` 12
