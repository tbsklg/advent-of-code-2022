module Day2Spec where

import Day2 
import Test.Hspec (Spec, it, shouldBe)

spec :: Spec
spec = do
  it "should find the maximum carriage of calories by an elf" $ do
    