module Day4Spec where

import Day4
  ( convertToAssignmentPair,
    fullyOverlaps,
    partiallyOverlaps,
    solve,
    solvePartTwo,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "camp cleanup" $ do
    it "should count how many ranges overlap" $ do
      solve ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"] `shouldBe` 2

    it "should convert to a pair of assignments" $ do
      convertToAssignmentPair "2-4,6-8" `shouldBe` ((2, 4), (6, 8))

    it "should check if one assignment fully overlaps the other" $ do
      fullyOverlaps ((2, 8), (3, 7)) `shouldBe` True
      fullyOverlaps ((2, 4), (6, 8)) `shouldBe` False

    it "should count in how many assignment pairs the ranges overlap" $ do
      solvePartTwo ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"] `shouldBe` 4

    it "should check if one assignment partially overlaps the other" $ do
      partiallyOverlaps ((2, 4), (6, 8)) `shouldBe` False
      partiallyOverlaps ((2, 3), (4, 5)) `shouldBe` False
      partiallyOverlaps ((5, 7), (7, 9)) `shouldBe` True
      partiallyOverlaps ((2, 8), (3, 7)) `shouldBe` True
      partiallyOverlaps ((6, 6), (4, 6)) `shouldBe` True
      partiallyOverlaps ((2, 6), (4, 8)) `shouldBe` True
