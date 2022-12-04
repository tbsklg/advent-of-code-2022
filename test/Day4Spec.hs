module Day4Spec where

import Day4
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "camp cleanup" $ do
    it "should count how many ranges overlap" $ do
      solve ["2-4,6-8", "2-3,4-5", "5-7,7-9", "2-8,3-7", "6-6,4-6", "2-6,4-8"] `shouldBe` 2

    it "should convert to a pair of assignments" $ do
      convertToAssignmentPair "2-4,6-8" `shouldBe` ((2, 4), (6, 8))

    it "should check one assignment fully overlaps the other" $ do
      overlap ((2, 8), (3, 7)) `shouldBe` True
      overlap ((2, 4), (6, 8)) `shouldBe` False
