module Day5Spec where

import Day5
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "supply stacks" $ do
    it "should load load the ship" $ do
        crates ["    [D]    ","[N] [C]    ","[Z] [M] [P]"] `shouldBe` [[" ", "N", "Z"], ["D","C","M"], [" "," ","P"]]
