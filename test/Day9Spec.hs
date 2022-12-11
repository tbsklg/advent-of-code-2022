module Day9Spec where

import Day9
  ( Direction (D, L, R, U),
    Motion (Motion),
    convertToMotions,
    moveTo,
    performMotion,
    performMotionWithNKnots,
    solve,
  )
import Test.Hspec (Spec, describe, it, shouldBe)

motions :: [[Char]]
motions = ["R 4", "U 4", "L 3", "D 1", "R 4", "D 1", "L 5", "R 2"]

spec :: Spec
spec = do
  describe "rope bridgte" $ do
    it "should calculate how many positions the tail of the rope visit at least once" $ do
      solve motions `shouldBe` 13

    it "move" $ do
      performMotion [(0, 0)] (Motion D 1) `shouldBe` [(0, -1), (0, 0)]
      performMotion [(0, 0)] (Motion D 2) `shouldBe` [(0, -2), (0, -1), (0, 0)]
      performMotion [(4, 0), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion U 1) `shouldBe` [(4, 1), (3, 0), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion U 1) `shouldBe` [(4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion U 1) `shouldBe` [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion U 1) `shouldBe` [(4, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(4, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion L 1) `shouldBe` [(3, 4), (4, 3), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion L 1) `shouldBe` [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion L 1) `shouldBe` [(1, 4), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(1, 4), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion D 1) `shouldBe` [(1, 3), (2, 4), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(1, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion R 1) `shouldBe` [(2, 3), (2, 4), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(2, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion R 1) `shouldBe` [(3, 3), (2, 4), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion R 1) `shouldBe` [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion R 1) `shouldBe` [(5, 3), (4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]
      performMotion [(5, 3), (4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)] (Motion D 1) `shouldBe` [(5, 2), (4, 3), (4, 3), (3, 3), (2, 4), (3, 4), (4, 3), (4, 2), (4, 1), (3, 0), (2, 0), (1, 0), (0, 0)]

    it "should perform a motion with 10 knots" $ do
      performMotionWithNKnots [(0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)] R `shouldBe` [(1, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]
      performMotionWithNKnots [(1, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)] R `shouldBe` [(2, 0), (1, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0), (0, 0)]

    it "should move towards" $ do
      moveTo (4, 0) (2, 0) `shouldBe` (3, 0)
      moveTo (0, -1) (0, 0) `shouldBe` (0, 0)
      moveTo (4, 1) (3, 0) `shouldBe` (3, 0)

    it "should covert to motions" $ do
      convertToMotions motions `shouldBe` [Motion R 4, Motion U 4, Motion L 3, Motion D 1, Motion R 4, Motion D 1, Motion L 5, Motion R 2]
