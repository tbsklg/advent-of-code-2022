module Day10Spec where

import Day10
import Test.Hspec (Spec, describe, it, shouldBe)

exampleProgram = ["addx 15","addx -11","addx 6","addx -3","addx 5","addx -1","addx -8","addx 13","addx 4","noop","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx 5","addx -1","addx -35","addx 1","addx 24","addx -19","addx 1","addx 16","addx -11","noop","noop","addx 21","addx -15","noop","noop","addx -3","addx 9","addx 1","addx -3","addx 8","addx 1","addx 5","noop","noop","noop","noop","noop","addx -36","noop","addx 1","addx 7","noop","noop","noop","addx 2","addx 6","noop","noop","noop","noop","noop","addx 1","noop","noop","addx 7","addx 1","noop","addx -13","addx 13","addx 7","noop","addx 1","addx -33","noop","noop","noop","addx 2","noop","noop","noop","addx 8","noop","addx -1","addx 2","addx 1","noop","addx 17","addx -9","addx 1","addx 1","addx -3","addx 11","noop","noop","addx 1","noop","addx 1","noop","noop","addx -13","addx -19","addx 1","addx 3","addx 26","addx -30","addx 12","addx -1","addx 3","addx 1","noop","noop","noop","addx -9","addx 18","addx 1","addx 2","noop","noop","addx 9","noop","noop","noop","addx -1","addx 2","addx -37","addx 1","addx 3","noop","addx 15","addx -21","addx 22","addx -6","addx 1","noop","addx 2","addx 1","noop","addx -10","noop","noop","addx 20","addx 1","addx 2","addx 2","addx -6","addx -11","noop","noop","noop"]

spec :: Spec
spec = do
  describe "cathode-ray Tube" $ do
    it "sum of these six signal strengths" $ do
      solve exampleProgram `shouldBe` 13140

    it "should parse the cpu instructions" $ do
      parse ["addx 15"] `shouldBe` [ADDX 15]
      parse ["addx 15", "noop", "addx 19", "addx -1"] `shouldBe` [ADDX 15, NOOP, ADDX 19, ADDX (-1)]
      parse ["addx 15", "addx -11", "addx 6", "addx -3", "addx 5", "addx -1", "addx -8"] `shouldBe` [ADDX 15, ADDX (-11), ADDX 6, ADDX (-3), ADDX 5, ADDX (-1), ADDX (-8)]

    it "should execute some cpu instructions" $ do
      execute [ADDX 15, ADDX (-11), ADDX 6, ADDX (-3), ADDX 5, ADDX (-1), ADDX (-8)]
        `shouldBe` [Register 1 (1, 1, 1), Register 2 (1, 1, 16), Register 3 (16, 16, 16), Register 4 (16, 16, 5), Register 5 (5, 5, 5), Register 6 (5, 5, 11), Register 7 (11, 11, 11), Register 8 (11, 11, 8), Register 9 (8, 8, 8), Register 10 (8, 8, 13), Register 11 (13, 13, 13), Register 12 (13, 13, 12), Register 13 (12, 12, 12), Register 14 (12, 12, 4)]

    it "should find the signal strength during the 20th, 60th, 100th, 140th, 180th, and 220th cycles" $ do
      signals [Register 1 (1, 1, 1), Register 2 (1, 1, 16), Register 20 (16, 16, 16), Register 4 (16, 16, 5), Register 60 (5, 5, 5), Register 6 (5, 5, 11), Register 100 (11, 11, 11), Register 8 (11, 11, 8), Register 140 (8, 8, 8), Register 10 (8, 8, 13), Register 180 (13, 13, 13), Register 12 (13, 13, 12), Register 13 (12, 12, 12), Register 220 (12, 12, 4)]
        `shouldBe` [Register 20 (16,16,16),Register 60 (5,5,5),Register 100 (11,11,11),Register 140 (8,8,8),Register 180 (13,13,13),Register 220 (12,12,4)]
    
    it "should sum up the signal strength" $ do
      signalStrength [Register 20 (16,21,16),Register 60 (5,19,5),Register 100 (11,18,11),Register 140 (8,21,8),Register 180 (13,16,13),Register 220 (12,18,4)] `shouldBe` 13140
