module Day6Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day6

spec :: Spec
spec = do
  describe "tuning trouble" $ do
    it "should find the start of a packet marker" $ do
        solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 7

    it "should check if the sub routine contains a marker" $ do
        isUnique "mjqj" `shouldBe` False
        isUnique "vwbj" `shouldBe` True
    
    it "should find the position of the marker" $ do
        findMarker 4 "vwbj" `shouldBe` 4
        