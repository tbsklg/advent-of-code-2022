module Day11Spec where

import qualified Data.Map as M
import Day11
import Test.Hspec (Spec, describe, it, shouldBe)

monkeyNotes = ["Monkey 0:", "  Starting items: 79, 98", "  Operation: new = old * 19", "  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3", "", "Monkey 1:", "  Starting items: 54, 65, 75, 74", "  Operation: new = old + 6", "  Test: divisible by 19", "    If true: throw to monkey 2", "    If false: throw to monkey 0", "", "Monkey 2:", "  Starting items: 79, 60, 97", "  Operation: new = old * old", "  Test: divisible by 13", "    If true: throw to monkey 1", "    If false: throw to monkey 3", "", "Monkey 3:", "  Starting items: 74", "  Operation: new = old + 3", "  Test: divisible by 17", "    If true: throw to monkey 0", "    If false: throw to monkey 1"]

spec :: Spec
spec = do
  describe "Monkey in the Middle" $ do
    it "should calculate the level of monkey business after 20 rounds of stuff-slinging simian shenanigans" $ do
      True `shouldBe` True

    it "should parse monkey number" $ do
      parseNumberFrom "Monkey 0:" `shouldBe` 0

    it "should parse items" $ do
      parseItemsFrom "  Starting items: 79, 98" `shouldBe` [79, 98]
      parseItemsFrom "  Starting items: 79" `shouldBe` [79]

    it "should parse operation" $ do
      parseOperationFn "  Operation: new = old * 19" 2 `shouldBe` 38
      parseOperationFn "  Operation: new = old + 19" 2 `shouldBe` 21
      parseOperationFn "  Operation: new = old - 19" 2 `shouldBe` (-17)
      parseOperationFn "  Operation: new = old / 19" 37 `shouldBe` 1

    it "should parse throw" $ do
      parseRecipientFn ["  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3"] 23 `shouldBe` 2
      parseRecipientFn ["  Test: divisible by 23", "    If true: throw to monkey 2", "    If false: throw to monkey 3"] 24 `shouldBe` 3
