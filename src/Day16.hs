{-# LANGUAGE InstanceSigs #-}

module Day16 where

import Data.Function (on)
import Data.List (maximumBy, nub, permutations, (\\))
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import qualified Data.Set as S
import Debug.Trace
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
    Right a -> print . bfs "AA" 30 . createTunnel $ a

createTunnel :: [Valve] -> Tunnel
createTunnel = foldl (\y x -> M.insert (name x) x y) M.empty

test :: Tunnel -> Int
test tunnel = traceShow distances maximum . map (\x -> overallPressure tunnel ("AA" : x) 30 [] $ distances) $ valvesToOpen
  where
    valvesToOpen = permutations . valvesWithFlowGreaterThanZero $ tunnel
    distances = distanceMatrix "AA" tunnel

type Cache = M.Map (Tunnel, Path, Time, [String], Distances) Int

overallPressure :: Tunnel -> Path -> Time -> [String] -> Distances -> Int
overallPressure tunnel path time opened distances = case path of
  [] -> 0
  [v] -> pressureFromVertex tunnel v time opened distances
  (v : vs) -> pressureFromVertex tunnel v (timeToOpen + timeToWalk) opened distances + overallPressure tunnel vs remainingTime (v : opened) distances
    where
      timeToOpen = 1
      remainingTime = max 0 (time - timeToOpen - timeToWalk)
      timeToWalk = fromJust . M.lookup (v, head vs) $ distances

pressureFromVertex :: Tunnel -> String -> Time -> [String] -> Distances -> Int
pressureFromVertex tunnel v time opened distances
  | time - 1 >= 0 && v `notElem` opened = time * currentPressure
  | otherwise = 0
  where
    currentPressure = sum . map (\x -> flowRate (tunnel M.! x)) $ (v : opened)

dfs :: String -> Int -> Tunnel -> Int
dfs start time tunnel = go start time [] (valvesWithFlowGreaterThanZero tunnel)
  where
    go _ 0 _ _ = 0
    go current time opened [_] = (sum . map (\x -> flowRate (tunnel M.! x)) $ nextOpened) * time
      where
        nextOpened = current : opened
    go current time opened toOpen
      | null toOpen || time < 0 || null possibilites = 0
      | otherwise =
        maximum
          . map (\(to, distance) -> pressure (distance + 1) + go to (time - 1 - distance) nextOpened nextToOpen)
          $ nextToVisit
      where
        pressure time = (sum . map (\x -> flowRate (tunnel M.! x)) $ nextOpened) * time
        nextToOpen = toOpen \\ [current]
        nextOpened = current : opened
        nextToVisit =
          [ x
            | x <- possibilites,
              fst x `notElem` opened
          ]

        possibilites = map (\((_, to), distance) -> (to, distance)) . filter (\((from, _), _) -> from == current) . M.toList $ distances

    distances = distanceMatrix start tunnel

distanceMatrix :: String -> M.Map String Valve -> M.Map (String, String) Int
distanceMatrix start tunnel = go M.empty (start : valvesWithFlowGreaterThanZero tunnel)
  where
    go distances [] = distances
    go distances (x : xs) = go (M.union newDistances distances) xs
      where
        newDistances = M.fromList [((x, y), fromJust . distance x y $ tunnel) | y <- M.keys tunnel \\ [x]]

distance :: String -> String -> Tunnel -> Maybe Int
distance start end tunnel = go [start] 0
  where
    go toVisit minute
      | null toVisit = Nothing
      | end `elem` toVisit = Just minute
      | otherwise = go newToVisit (minute + 1)
      where
        newToVisit =
          [ x
            | x <- concatMap (neighbours . (tunnel M.!)) toVisit,
              x `notElem` toVisit
          ]

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
