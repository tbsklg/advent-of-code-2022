module Day16Spec where

import Day16
import Test.Hspec (Spec, describe, it, shouldBe)
import qualified Text.Parsec as P

testValves = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\nValve HH has flow rate=22; tunnel leads to valve GG\n"

spec :: Spec
spec = do
  describe "Proboscidea Volcanium" $ do
    it "should parse valves" $ do
      parseValves "" testValves `shouldBe` Right [Valve {name = "AA", flowRate = 0, neighbours = ["DD", "II", "BB"]}, Valve {name = "HH", flowRate = 22, neighbours = ["GG"]}]
