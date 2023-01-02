{-# LANGUAGE InstanceSigs #-}

module Day16 where

import Data.Function (on)
import Data.List (maximumBy, nub, permutations, (\\))
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

data DFS = DFS {queue :: [String], opened :: [String], minute :: Int, pressure :: Int, visited :: [String]} deriving (Show, Eq)

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

dfs' :: String -> Int -> Tunnel -> Int
dfs' start time tunnel = guenther start start time $ S.singleton start
  where
    guenther current elephant time opened
      | time < 1 = 0
      | null possibilities = pressure time opened
      | otherwise =
          maximum
          . map
            ( \((current, dc), (elephant, de)) ->
                pressure (min (max dc de + 2) time ) nextOpened
                  + guenther current elephant (time - 2 - max dc de) nextOpened
            )
          $ possibilities
      where
        nextOpened = S.insert elephant . S.insert current $ opened

        possibilities = nub . concatMap (\x -> [(x, y) |
           fst x `notElem` nextOpened,
           y <- elephantPossibilities,
           fst y `notElem` nextOpened]) $ currentPossibilities

        currentPossibilities = [x | x <- S.toList $ distances M.! current]
        elephantPossibilities = [x | x <- S.toList $ distances M.! elephant]

    pressure time opened = (sum . map (\x -> flowRate (tunnel M.! x)) . S.toList $ opened) * time
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
          . map (\(to, distance) -> pressure (min (distance + 1) time) + go to (time - 1 - distance) nextOpened)
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
    updateMatrix from matrix to
      | from == to = M.empty
      | otherwise = M.unionWith S.union matrix distances
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
