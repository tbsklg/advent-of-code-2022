module Day6Spec where

import Test.Hspec (Spec, describe, it, shouldBe)
import Day6
import Day6 (containsMarker)

spec :: Spec
spec = do
  describe "tuning trouble" $ do
    it "should find the start of a packet marker" $ do
        solve "mjqjpqmgbljsphdztnvjfqwrcgsmlb" `shouldBe` 5

    it "should check if the sub routine contains a marker" $ do
        containsMarker "mjqj" `shouldBe` False
        containsMarker "vwbj" `shouldBe` True
    
    it "should find the position of the marker" $ do
        findMarker "vwbj" `shouldBe` 4
        