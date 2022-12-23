module Day16Spec where

import qualified Data.Map as M
import qualified Data.Set as S
import Day16
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.Parsec as P

testValves = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve HH has flow rate=22; tunnel leads to valve GG\n"

tunnel :: M.Map [Char] Valve
tunnel =
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

spec :: Spec
spec = do
  describe "Proboscidea Volcanium" $ do
    it "should parse valves" $ do
      parseValves "" testValves `shouldBe` Right [Valve {name = "AA", flowRate = 0, neighbours = ["DD", "II", "BB"]}, Valve {name = "HH", flowRate = 22, neighbours = ["GG"]}]

    it "should find valves with flow rate greater than zero" $ do
      valvesWithFlowGreaterThanZero tunnel `shouldBe` ["BB", "CC", "DD", "EE", "HH", "JJ"]

    it "should walk within the tunnel" $ do
      minDistance "AA" "DD" tunnel `shouldBe` Just 1