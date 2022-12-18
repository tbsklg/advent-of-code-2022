module Day15Spec where

import Day15
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "Distress Signal" $ do
    it "should parse sensors" $ do
      parse
        [ "Sensor at x=2, y=18: closest beacon is at x=-2, y=15",
          "Sensor at x=9, y=16: closest beacon is at x=10, y=16",
          "Sensor at x=8, y=7: closest beacon is at x=2, y=10",
          "Sensor at x=8, y=7: closest beacon is at x=-2, y=10"
        ]
        `shouldBe` [ Sensor {position = (2, 18), closestBeacon = (-2, 15)},
                     Sensor {position = (9, 16), closestBeacon = (10, 16)},
                     Sensor {position = (8, 7), closestBeacon = (2, 10)},
                     Sensor {position = (8, 7), closestBeacon = (-2, 10)}
                   ]

    it "should find all scanning postions for a target y position" $ do
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} 4 `shouldBe` Just (0, 4)
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} 5 `shouldBe` Just (1, 3)
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} 6 `shouldBe` Just (2, 2)
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} 7 `shouldBe` Nothing
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} (-4) `shouldBe` Nothing
      scanningRange Sensor {position = (2, 2), closestBeacon = (-2, 2)} (-1) `shouldBe` Just (1, 3)
      scanningRange Sensor {position = (16, 7), closestBeacon = (15, 3)} 10 `shouldBe` Just (14, 18)
