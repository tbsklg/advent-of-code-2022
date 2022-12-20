module Day16 where

import qualified Text.Parsec as P
import Text.Parsec.Char (letter)
import Text.Parsec.Combinator (sepBy)

data Valve = Valve {name :: String, flowRate :: Int, neighbours :: [String]} deriving (Show, Eq)

solve :: IO String -> IO ()
solve xs = do
  result <- parseValves "" <$> xs
  case result of
    Left a -> error "Input parsing not working!"
    Right a -> putStrLn ("Parse OK!" ++ show a)

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
