module Day2Spec where

import Day2 (solve, solvePartTwo, rpsForDesiredResult, resultForGame, RPS(..), Result(..), Game(..))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "rock paper scissors" $ do
    it "should calculate the score when playing with a strategy guide" $ do
      solve ["A Y", "B X", "C Z"] `shouldBe` 15

    it "should calculate the score when playing with a ultra secret strategy guide" $ do
      solvePartTwo ["A Y", "B X", "C Z"] `shouldBe` 12

    it "should return the result for a given game" $ do
      resultForGame (Rock, Rock) `shouldBe` Draw
      resultForGame (Rock, Paper) `shouldBe` Win
      resultForGame (Rock, Scissor) `shouldBe` Loss
      resultForGame (Paper, Rock) `shouldBe` Loss
      resultForGame (Paper, Paper) `shouldBe` Draw
      resultForGame (Paper, Scissor) `shouldBe` Win
      resultForGame (Scissor, Rock) `shouldBe` Win
      resultForGame (Scissor, Paper) `shouldBe` Loss
      resultForGame (Scissor, Scissor) `shouldBe` Draw

    it "should return the RPS for a given opponent RPS and a desirec result" $ do
      rpsForDesiredResult (Rock, Loss) `shouldBe` Scissor
      rpsForDesiredResult (Rock, Draw) `shouldBe` Rock
      rpsForDesiredResult (Rock, Win) `shouldBe` Paper
      rpsForDesiredResult (Paper, Loss) `shouldBe` Rock
      rpsForDesiredResult (Paper, Draw) `shouldBe` Paper
      rpsForDesiredResult (Paper, Win) `shouldBe` Scissor
      rpsForDesiredResult (Scissor, Loss) `shouldBe` Paper
      rpsForDesiredResult (Scissor, Draw) `shouldBe` Scissor
      rpsForDesiredResult (Scissor, Win) `shouldBe` Rock 