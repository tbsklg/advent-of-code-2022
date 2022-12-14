module Day13Spec where

import Day13
import Test.Hspec (Spec, describe, it, shouldBe)

testSignal = ["[1,1,3,1,1]", "[1,1,5,1,1]", "", "[[1],[2,3,4]]", "[[1],4]", "", "[9]", "[[8,7,6]]", "", "[[4,4],4,4]", "[[4,4],4,4,4]", "", "[7,7,7,7]", "[7,7,7]", "", "[]", "[3]", "", "[[[]]]", "[[]]", "", "[1,[2,[3,[4,[5,6,7]]]],8,9]", "[1,[2,[3,[4,[5,6,0]]]],8,9]"]

spec :: Spec
spec = do
  describe "Distress Signal" $ do
    it "should  sum of the indices of those pairs" $
      solve testSignal `shouldBe` 13
    
    it "should order all packages" $ do
      solvePartTwo testSignal `shouldBe` 140

    it "should compare two lists" $ do
      compareSignal "[1,1,3,1,1]" "[1,1,5,1,1]" `shouldBe` True
      compareSignal "[[1],[2,3,4]]" "[[1],4]" `shouldBe` True
      compareSignal "[9]" "[[8,7,6]]" `shouldBe` False
      compareSignal "[[4,4],4,4]" "[[4,4],4,4,4]" `shouldBe` True
      compareSignal "[7,7,7,7]" "[7,7,7]" `shouldBe` False
      compareSignal "[]" "[3]" `shouldBe` True
      compareSignal "[[[]]]" "[[]]" `shouldBe` False
      compareSignal "[1,[2,[3,[4,[5,6,7]]]],8,9]" "[1,[2,[3,[4,[5,6,0]]]],8,9]" `shouldBe` False
      compareSignal "[1,[2,[3,[4,[5,6,7]]]],8,9,99999]" "[1,[2,[3,[4,[5,6,7]]]],8,9,1000000]" `shouldBe` True
      compareSignal "[4]" "[2, 2, 2]" `shouldBe` False
      compareSignal "[]" "[[[[5,10]]]]" `shouldBe` True
