module Day8Spec where

import Day8
import Test.Hspec (Spec, describe, it, shouldBe)

forest = ["30373","25512","65332","33549","35390"]

spec :: Spec
spec = do
  describe "treetop tree house" $ do
    it "should calculate how many trees are visible from outside the grid" $ do
        solve forest `shouldBe` 21
    
    it "should calculate outer trees" $ do
        outerTrees forest `shouldBe` 16

    it "should calculate the bootom trees" $ do
        bottomTreesFrom (1,1) forest `shouldBe` "535"
        bottomTreesFrom (3,3) forest `shouldBe` "9"
    
    it "should calculate the top trees" $ do
        topTreesFrom (1,1) forest `shouldBe` "0"
        topTreesFrom (3,3) forest `shouldBe` "317"

    it "should calculate the right trees" $ do
        rightTreesFrom (2,2) forest `shouldBe` "32"
        rightTreesFrom (3,3) forest `shouldBe` "9"
    
    it "should calculate the left trees" $ do
        leftTreesFrom (2,2) forest `shouldBe` "56"

    it "should find the highest scenic score possible" $ do
        solvePartTwo forest `shouldBe` 8
