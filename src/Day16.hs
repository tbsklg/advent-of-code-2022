{-# LANGUAGE InstanceSigs #-}

module Day16 where

import Data.Function (on)
import Data.List (intersect, maximumBy, nub, permutations, tails, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace (traceShow)
import qualified Text.Parsec as P
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (sepBy)

data Valve = Valve {name :: String, flowRate :: Int, neighbours :: [String]} deriving (Show, Eq)

type Tunnel = M.Map String Valve

type Distances = M.Map (String, String) Int

type Path = [String]

type Time = Int

data DFS = DFS {queue :: [String], opened :: [String], time :: Int, overallPressure :: Int} deriving (Show, Eq)

instance Ord Valve where
  compare :: Valve -> Valve -> Ordering
  compare v1 v2 = compare (flowRate v1) (flowRate v2)

solve :: IO String -> IO ()
solve xs = do
  result <- parseValves "" <$> xs
  case result of
    Left a -> error "Input parsing not working!"
    Right a -> print . dfs "AA" 30 . createTunnel $ a

solvePartTwo :: IO String -> IO ()
solvePartTwo xs = do
  result <- parseValves "" <$> xs
  case result of
    Left a -> error "Input parsing not working!"
    Right a -> print . dfs' "AA" 26 . createTunnel $ a

createTunnel :: [Valve] -> Tunnel
createTunnel = foldl (\y x -> M.insert (name x) x y) M.empty

dfs' start time tunnel = result
  where
    me = guenther (DFS [start] [] time 0) tunnel
    result =
      maximum
        [ v1 + v2 | (open1, v1) : elephants <- tails (M.toList me), 
        (open2, v2) <- elephants, 
        S.disjoint (S.fromList (open1 \\ ["AA"])) (S.fromList (open2 \\ ["AA"]))
        ]

guenther :: DFS -> Tunnel -> M.Map [String] Int
guenther dfs tunnel = go dfs
  where
    go :: DFS -> M.Map [String] Int
    go (DFS [] _ _ _) = M.empty
    go (DFS (current : queue) opened time overallPressure)
      | time < 1 = M.singleton opened overallPressure
      | null nextToVisit = M.singleton nextOpened (overallPressure + pressure time nextOpened)
      | otherwise =
        M.unions
          [ M.singleton opened (overallPressure + pressure time opened),
            M.unions
              . map
                ( \(to, distance) ->
                    go
                      ( DFS
                          (queue ++ [to])
                          nextOpened
                          (time - 1 - distance)
                          (overallPressure + pressure (min (distance + 1) time) nextOpened)
                      )
                )
              $ nextToVisit
          ]
      where
        nextOpened = opened ++ [current]
        nextToVisit =
          [ x
            | x <- possibilites,
              fst x `notElem` nextOpened
          ]
        possibilites = S.toList $ distances M.! current

    pressure time opened = (sum . map (\x -> flowRate (tunnel M.! x)) $ opened) * time
    distances = distanceMatrix "AA" tunnel

dfs :: String -> Int -> Tunnel -> Int
dfs start time tunnel = go start time []
  where
    go _ 0 _ = 0
    go current time opened
      | time < 1 = 0
      | null nextToVisit = pressure time
      | otherwise =
        maximum
          . map
            ( \(to, distance) ->
                pressure (min (distance + 1) time)
                  + go to (time - 1 - distance) nextOpened
            )
          $ nextToVisit
      where
        pressure time = (sum . map (\x -> flowRate (tunnel M.! x)) $ nextOpened) * time
        nextOpened = current : opened
        nextToVisit =
          [ x
            | x <- possibilites,
              fst x `notElem` nextOpened
          ]

        possibilites = S.toList $ distances M.! current

    distances = distanceMatrix "AA" tunnel

distanceMatrix :: String -> M.Map String Valve -> M.Map String (S.Set (String, Int))
distanceMatrix start tunnel = go valves M.empty
  where
    valves = start : valvesWithFlowGreaterThanZero tunnel

    go :: [String] -> M.Map String (S.Set (String, Int)) -> M.Map String (S.Set (String, Int))
    go [] matrix = matrix
    go (valve : vs) matrix = go vs (foldl (updateMatrix valve) matrix vs)

    updateMatrix :: String -> M.Map String (S.Set (String, Int)) -> String -> M.Map String (S.Set (String, Int))
    updateMatrix from matrix to = M.unionWith S.union matrix distances
      where
        distance = fromJust . bfs from to $ tunnel
        distances = M.fromList [(from, S.singleton (to, distance)), (to, S.singleton (from, distance))]

bfs :: String -> String -> Tunnel -> Maybe Int
bfs source dest tunnel
  | source == dest = Just 0
  | otherwise = go (S.singleton source) S.empty 0
  where
    go queue visited distance
      | S.null queue = Nothing
      | dest `elem` adjacentNeighbours = Just (distance + 1)
      | otherwise = go nextQueue (S.union visited (S.fromList adjacentNeighbours)) (distance + 1)
      where
        adjacentNeighbours =
          [ x
            | v <- S.toList queue,
              x <- neighbours $ tunnel M.! v,
              not (x `S.member` visited)
          ]
        nextQueue = S.fromList adjacentNeighbours

valvesWithFlowGreaterThanZero :: Tunnel -> [String]
valvesWithFlowGreaterThanZero tunnel = filter (\x -> flowRate (tunnel M.! x) > 0) (M.keys tunnel)

parseInt :: P.Parsec String () Int
parseInt = (negate <$> (P.char '-' *> (read <$> P.many1 P.digit))) P.<|> (read <$> P.many1 P.digit)

parseValves :: P.SourceName -> String -> Either P.ParseError [Valve]
parseValves = P.parse $ parseValve `P.endBy` P.newline <* P.eof
  where
    parseValve = do
      name <- P.string "Valve " *> P.many letter
      flowRate <- P.string " has flow rate=" *> parseInt
      P.string "; tunnel" *> P.many (P.char 's') *> P.string " lead"
        *> P.many (P.char 's')
        *> P.string " to valve"
        *> P.many (P.char 's')
        *> P.string " "
      neighbours <- P.many letter `sepBy` P.string ", "
      return Valve {name = name, flowRate = flowRate, neighbours = neighbours}
