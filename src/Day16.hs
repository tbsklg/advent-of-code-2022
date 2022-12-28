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

solve :: IO String -> IO ()
solve xs = do
  result <- parseValves "" <$> xs
  case result of
    Left a -> error "Input parsing not working!"
    Right a -> print . test . createTunnel $ a

createTunnel :: [Valve] -> Tunnel
createTunnel = foldl (\y x -> M.insert (name x) x y) M.empty

test :: Tunnel -> Int
test tunnel = maximum . map (\x -> overallPressure tunnel ("AA": x) 30 [] distances) $ valvesToOpen
  where
    valvesToOpen = permutations . valvesWithFlowGreaterThanZero $ tunnel
    distances = distanceMatrix "AA" tunnel

overallPressure :: Tunnel -> Path -> Time -> [String] -> Distances -> Int
overallPressure tunnel [] _ _ _  = 0
overallPressure tunnel [v] minutes opened distances
      | minutes - 1 >= 0 && v `notElem` opened = nextPressure + overallPressure tunnel [] minutes (v:opened) distances
      | otherwise = 0
      where
        nextPressure = minutes * (sum . map (\x -> flowRate (tunnel M.! x)) $ (v:opened))
overallPressure tunnel (v:vs) minutes opened distances
      | minutes - 1 - dist > 0 && v `notElem` opened = nextPressure + overallPressure tunnel vs (minutes - 1 - dist) (v:opened) distances
      | v `notElem` opened = flowRate (tunnel M.! v)
      | otherwise = 0
      where
        nextPressure = (dist + 1) * (sum . map (\x -> flowRate (tunnel M.! x)) $ (v:opened))

        dist = fromJust . M.lookup (v, head vs) $ distances

distanceMatrix :: String -> M.Map String Valve -> M.Map (String, String) Int
distanceMatrix start tunnel = go M.empty (start : valvesWithFlowGreaterThanZero tunnel)
  where
    go distances [] = distances
    go distances (x : xs) = go (M.union newDistances distances) xs
      where
        newDistances = M.fromList [((x, y), fromJust . distance x y $ tunnel) | y <- M.keys tunnel \\ [x]]

distance :: String -> String -> Tunnel -> Maybe Int
distance start end tunnel = go [start] 0 []
  where
    go toVisit minute visited
      | null toVisit = Nothing
      | end `elem` toVisit = Just minute
      | otherwise = go newToVisit (minute + 1) newVisited
      where
        newToVisit =
          [ x
            | x <- concatMap (neighbours . (tunnel M.!)) toVisit,
              x `notElem` visited
          ]
        newVisited = visited ++ newToVisit

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
