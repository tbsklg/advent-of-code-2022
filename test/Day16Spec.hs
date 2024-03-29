module Day16Spec where

import qualified Data.Map as M
import qualified Data.Set as S
import Day16
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.Parsec as P

testValves :: [Char]
testValves = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve HH has flow rate=22; tunnel leads to valve GG\n"

bigTunnel :: M.Map [Char] Valve
bigTunnel =
  M.fromList
    [ ("AA", Valve {name = "AA", flowRate = 0, neighbours = ["DD", "II", "BB"]}),
      ("BB", Valve {name = "BB", flowRate = 13, neighbours = ["CC", "AA"]}),
      ("CC", Valve {name = "CC", flowRate = 2, neighbours = ["DD", "BB"]}),
      ("DD", Valve {name = "DD", flowRate = 20, neighbours = ["CC", "AA", "EE"]}),
      ("EE", Valve {name = "EE", flowRate = 3, neighbours = ["FF", "DD"]}),
      ("FF", Valve {name = "FF", flowRate = 0, neighbours = ["EE", "GG"]}),
      ("GG", Valve {name = "GG", flowRate = 0, neighbours = ["FF", "HH"]}),
      ("HH", Valve {name = "HH", flowRate = 22, neighbours = ["GG"]}),
      ("II", Valve {name = "II", flowRate = 0, neighbours = ["AA", "JJ"]}),
      ("JJ", Valve {name = "JJ", flowRate = 21, neighbours = ["II"]})
    ]

simpleTunnel :: M.Map [Char] Valve
simpleTunnel =
  M.fromList
    [ ("AA", Valve {name = "AA", flowRate = 1, neighbours = ["CC", "DD"]}),
      ("CC", Valve {name = "CC", flowRate = 2, neighbours = ["AA", "BB"]}),
      ("BB", Valve {name = "BB", flowRate = 3, neighbours = ["CC", "DD"]}),
      ("DD", Valve {name = "DD", flowRate = 3, neighbours = ["BB", "AA"]})
    ]

spec :: Spec
spec = do
  describe "Proboscidea Volcanium" $ do
    it "should parse valves" $ do
      parseValves "" testValves `shouldBe` Right [Valve {name = "AA", flowRate = 0, neighbours = ["DD", "II", "BB"]}, Valve {name = "HH", flowRate = 22, neighbours = ["GG"]}]

    it "should find valves with flow rate greater than zero" $ do
      valvesWithFlowGreaterThanZero bigTunnel `shouldBe` ["BB", "CC", "DD", "EE", "HH", "JJ"]

  describe "minDistance" $ do
    it "should return Nothing when start and end nodes are not connected" $ do
      bfs "AA" "ZZ" bigTunnel `shouldBe` Nothing

    it "should return the correct distance when start and end nodes are connected" $ do
      bfs "AA" "CC" bigTunnel `shouldBe` Just 2

    it "should return the correct distance when start and end nodes are the same" $ do
      bfs "AA" "AA" bigTunnel `shouldBe` Just 0

    it "should return the correct distance when there are multiple paths to the end node" $ do
      bfs "BB" "DD" bigTunnel `shouldBe` Just 2
      bfs "HH" "II" bigTunnel `shouldBe` Just 6
      bfs "II" "JJ" bigTunnel `shouldBe` Just 1

  describe "allPaths" $ do
    it "should return all paths" $ do
      allPaths (State {queue = ["AA"], opened = [], time = 26, overallPressure = 0}) simpleTunnel
        `shouldBe` M.fromList
          [ ([], 0),
            (["AA"], 26),
            (["AA", "BB"], 95),
            (["AA", "BB", "CC", "DD"], 191),
            (["AA", "BB", "DD", "CC"], 194),
            (["AA", "CC"], 74),
            (["AA", "CC", "BB", "DD"], 200),
            (["AA", "CC", "DD", "BB"], 194),
            (["AA", "DD"], 98),
            (["AA", "DD", "BB", "CC"], 204),
            (["AA", "DD", "CC", "BB"], 197)
          ]
  
  describe "findMaxPressure" $ do
    it "should return the max pressure" $ do
      findMaxPressure "AA" 30 simpleTunnel `shouldBe` 240
      findMaxPressure "AA" 30 bigTunnel `shouldBe` 1651
      findMaxPressure' "AA" 26 simpleTunnel `shouldBe` 230
      findMaxPressure' "AA" 26 bigTunnel `shouldBe` 1707
