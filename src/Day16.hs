module Day16 where

import Data.List (nub)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import qualified Text.Parsec as P
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (sepBy)

data Valve = Valve {name :: String, flowRate :: Int, neighbours :: [String]} deriving (Show, Eq)

type Tunnel = M.Map String Valve

data DFS = DFS {queue :: [String], opened :: [String], minute :: Int, pressure :: Int} deriving (Show, Eq)

solve :: IO String -> IO ()
solve xs = do
  result <- parseValves "" <$> xs
  case result of
    Left a -> error "Input parsing not working!"
    Right a -> print a

minDistance :: String -> String -> M.Map String Valve -> Maybe Int
minDistance start end tunnel = go (S.singleton start) 0 (S.singleton start)
  where
    go toVisit minute visited
      | S.null toVisit = Nothing
      | S.member end toVisit = Just minute
      | otherwise = go newToVisit (minute + 1) newVisited
      where
        newToVisit =
          S.difference
            ( S.unions
                ( map
                    ( \x ->
                        S.fromList (neighbours (tunnel M.! x))
                    )
                    (S.toList toVisit)
                )
            )
            visited
        newVisited = S.union visited newToVisit

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
