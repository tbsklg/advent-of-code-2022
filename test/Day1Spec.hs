module Day1Spec where

import Day1 (solve, solvePartTwo)
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should find the maximum carriage of calories by an elf" $ do
    solve ["123", "", "456", "789", ""] `shouldBe` 1245
    solve ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000"] `shouldBe` 24000
  
  it "should find the sum of top three Elves carrying the most Calories" $ do
    solvePartTwo ["1000", "2000", "3000", "", "4000", "", "5000", "6000", "", "7000", "8000", "9000", "", "10000"] `shouldBe` 45000


