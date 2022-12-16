module Day14Spec where

import Day14
import Test.Hspec (Spec, describe, it, shouldBe)

testTrace :: [[Char]]
testTrace = ["498,4 -> 498,6 -> 496,6", "503,4 -> 502,4 -> 502,9 -> 494,9"]

testGrid :: [[Char]]
testGrid = ["..........", "..........", "..........", "..........", ".........."]

spec :: Spec
spec = do
  describe "Distress Signal" $ do
    it "should work" $ do
      True `shouldBe` True

    it "should parse scan traces" $ do
      parse testTrace
        `shouldBe` [ (Path {from = (498, 4), to = (498, 6)}),
                     (Path {from = (498, 6), to = (496, 6)}),
                     (Path {from = (503, 4), to = (502, 4)}),
                     (Path {from = (502, 4), to = (502, 9)}),
                     (Path {from = (502, 9), to = (494, 9)})
                   ]

    it "should normalize a list of path" $ do
      normalize
        [ (Path {from = (498, 4), to = (498, 6)}),
          (Path {from = (498, 6), to = (496, 6)}),
          (Path {from = (503, 4), to = (502, 4)}),
          (Path {from = (502, 4), to = (502, 9)}),
          (Path {from = (502, 9), to = (494, 9)})
        ]
        (500, 0)
        `shouldBe` ( [ Path {from = (4, 4), to = (4, 6)},
                       Path {from = (4, 6), to = (2, 6)},
                       Path {from = (9, 4), to = (8, 4)},
                       Path {from = (8, 4), to = (8, 9)},
                       Path {from = (8, 9), to = (0, 9)}
                     ],
                     (6, 0)
                   )

    it "should replace air with a rock within a grid" $ do
      replace (0, 0) '#' testGrid `shouldBe` ["#.........", "..........", "..........", "..........", ".........."]
      replace (1, 0) '#' testGrid `shouldBe` [".#........", "..........", "..........", "..........", ".........."]
      replace (1, 1) '#' testGrid `shouldBe` ["..........", ".#........", "..........", "..........", ".........."]
      replace (9, 4) '#' testGrid `shouldBe` ["..........", "..........", "..........", "..........", ".........#"]

    it "should create an empty grid" $ do
      emptyGridFrom
        [ Path {from = (4, 4), to = (4, 6)},
          Path {from = (4, 6), to = (2, 6)},
          Path {from = (9, 4), to = (8, 4)},
          Path {from = (8, 4), to = (8, 9)},
          Path {from = (8, 9), to = (0, 9)}
        ]
        `shouldBe` ["..........", "..........", "..........", "..........", "..........", "..........", "..........", "..........", "..........", ".........."]

    it "should create a grid with a pouring position" $ do
      grid
        (500, 0)
        [ (Path {from = (498, 4), to = (498, 6)}),
          (Path {from = (498, 6), to = (496, 6)}),
          (Path {from = (503, 4), to = (502, 4)}),
          (Path {from = (502, 4), to = (502, 9)}),
          (Path {from = (502, 9), to = (494, 9)})
        ]
        `shouldBe` ( (6, 0),
                     [ "..........",
                       "..........",
                       "..........",
                       "..........",
                       "....#...##",
                       "....#...#.",
                       "..###...#.",
                       "........#.",
                       "........#.",
                       "#########."
                     ]
                   )

    it "should perform one down falling step" $ do
      furtherFallings
        ( (6, 0),
          [ "..........",
            "..........",
            "..........",
            "..........",
            "....#...##",
            "....#...#.",
            "..###...#.",
            "........#.",
            "......o.#.",
            "#########."
          ]
        )
        `shouldBe` ( (6, 0),
                     [ "..........",
                       "..........",
                       "..........",
                       "..........",
                       "....#...##",
                       "....#...#.",
                       "..###...#.",
                       "........#.",
                       "......o.#.",
                       "#########."
                     ]
                   )

        