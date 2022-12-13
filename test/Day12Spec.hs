module Day12Spec where

import qualified Data.Map as M
import qualified Data.Set as S
import Day12
import Test.Hspec (Spec, describe, it, shouldBe)

testGrid :: [[Char]]
testGrid = ["Sabqponm", "abcryxxl", "accszExk", "acctuvwj", "abdefghi"]

spec :: Spec
spec = do
  describe "Hill Climbing Algorithm" $ do
    it "should calculate the fewest steps required to move from your current position to the location that should get the best signal" $ do
      solve testGrid `shouldBe` 31

    it "should calculate the fewest steps required to move starting from any square with elevation a to the location that should get the best signal" $ do
      solvePartTwo testGrid `shouldBe` 23

    it "should find all neighbours which are at most one higher" $ do
      adjacentNeighbours S.empty (0, 0) testGrid isValidElevationForwards `shouldBe` [(0, 1), (1, 0)]
      adjacentNeighbours S.empty (4, 0) testGrid isValidElevationForwards `shouldBe` [(4, 1), (3, 0)]
      adjacentNeighbours S.empty (4, 4) testGrid isValidElevationForwards `shouldBe` [(4, 3), (4, 5)]
      adjacentNeighbours S.empty (0, 4) testGrid isValidElevationForwards `shouldBe` [(0, 3), (0, 5)]
      adjacentNeighbours S.empty (0, 3) testGrid isValidElevationForwards `shouldBe` [(0, 2), (0, 4), (1, 3)]
      adjacentNeighbours S.empty (4, 2) testGrid isValidElevationForwards `shouldBe` [(4, 1), (4, 3), (3, 2)]
      adjacentNeighbours (S.fromList [(4, 3)]) (4, 2) testGrid isValidElevationForwards `shouldBe` [(4, 1), (3, 2)]

    it "should retrieve current eleveation from location" $ do
      elevation ["Sabqponm", "abcryxxl"] (0, 0) `shouldBe` 'S'
      elevation ["Sabqponm", "abcryxxl"] (1, 0) `shouldBe` 'a'
      elevation testGrid (4, 1) `shouldBe` 'b'
      elevation testGrid (4, 2) `shouldBe` 'd'

    it "should test for valid elevation" $ do
      isValidElevationForwards 'a' 'b' `shouldBe` True
      isValidElevationForwards 'S' 'x' `shouldBe` True
      isValidElevationForwards 'x' 'S' `shouldBe` False
      isValidElevationForwards 'b' 'c' `shouldBe` True
      isValidElevationForwards 'd' 'e' `shouldBe` True
