module Day9Spec where

import Day9
import Test.Hspec (Spec, describe, it, shouldBe)

motions = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]

spec :: Spec
spec = do
  describe "rope bridgte" $ do
    -- it "should calculate how many positions the tail of the rope visit at least once" $ do
    --   solve motions `shouldBe` 13

    it "should covert to motions" $ do
      convertToMotions motions `shouldBe` [Motion R 4, Motion U 4, Motion L 3, Motion D 1, Motion R 4, Motion D 1, Motion L 5, Motion R 2]

    it "should move" $ do
      move ((0, 0), []) (Motion R 4) `shouldBe` ((4, 0), [(3, 0), (2, 0), (1, 0), (0, 0)])
      move ((0, 0), []) (Motion D 1) `shouldBe` ((0, -1), [(0, 0)])
      move ((0, 0), []) (Motion D 2) `shouldBe` ((0, -2), [(0, -1), (0, 0)])
      move ((0, 0), []) (Motion L 1) `shouldBe` ((-1, 0), [(0, 0)])
      move ((0, 0), []) (Motion U 1) `shouldBe` ((0, 1), [(0, 0)])
      move ((4, 0), [(3, 0), (2, 0), (1, 0), (0, 0)]) (Motion U 1) `shouldBe` ((4, 1), [(3, 0), (2, 0), (1, 0), (0, 0)])
      move ((4, 1), [(3, 0), (2, 0), (1, 0), (0, 0)]) (Motion U 1) `shouldBe` ((4, 2), [(4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((4, 2), [(4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion U 1) `shouldBe` ((4, 3), [(4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((4, 3), [(4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion U 1) `shouldBe` ((4, 4), [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((4, 4), [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion L 1) `shouldBe` ((3, 4), [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((3, 4), [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion L 1) `shouldBe` ((2, 4), [(3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((2, 4), [(3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion L 1) `shouldBe` ((1, 4), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((1, 4), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion D 1) `shouldBe` ((1, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((1, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion R 1) `shouldBe` ((2, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((2, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion R 1) `shouldBe` ((3, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((3, 3), [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion R 1) `shouldBe` ((4, 3), [(3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((4, 3), [(3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion R 1) `shouldBe` ((5, 3), [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])
      move ((5, 3), [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]) (Motion D 1) `shouldBe` ((5, 2), [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)])

    it "should check if two ropes are connected" $ do
      isConnected (2, 0) (1, 0) `shouldBe` True
      isConnected (2, 0) (2, 0) `shouldBe` True
      isConnected (2, 0) (1, 1) `shouldBe` True
      isConnected (2, 0) (1, -1) `shouldBe` True
      isConnected (2, 0) (3, 1) `shouldBe` True
      isConnected (2, 0) (3, 0) `shouldBe` True
      isConnected (2, 0) (4, 0) `shouldBe` False
      isConnected (4, 2) (3, 0) `shouldBe` False

    it "should count all positions the tail visited at least once" $ do
      visitedOnce [(4, 3), (4, 3)] `shouldBe` [(4, 3)]
      visitedOnce [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] `shouldBe` [(0, 0), (1, 0), (2, 0), (2, 4), (3, 0), (3, 3), (3, 4), (4, 1), (4, 2), (4, 3)]
