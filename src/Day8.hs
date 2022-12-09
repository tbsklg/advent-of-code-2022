module Day8 where

type Forest = [String]

type Position = (Int, Int)

solve :: Forest -> Int
solve forest = outerTrees forest + innerTrees forest

solvePartTwo :: Forest -> Int
solvePartTwo = scenicView

outerTrees :: Forest -> Int
outerTrees xs = (length xs - 2) * 2 + (length xs * 2)

scenicView :: Forest -> Int
scenicView forest = maximum . map scencicScore . walk $ forest
  where
    scencicScore position@(x, y) = product . map countTrees $ surroundingTrees position forest
      where
        countTrees [] = 0
        countTrees (x : xs)
          | tree > x = 1 + countTrees xs
          | otherwise = 1

        tree = forest !! x !! y

innerTrees :: Forest -> Int
innerTrees forest = length . filter isVisible . walk $ forest
  where
    isVisible position@(x, y) = foldl (\y x -> y || all (tree >) x) False (surroundingTrees position forest)
      where
        tree = forest !! x !! y

surroundingTrees :: Position -> [String] -> [String]
surroundingTrees position forest = [top, bottom, left, right]
  where
    top = topTreesFrom position forest
    bottom = bottomTreesFrom position forest
    left = leftTreesFrom position forest
    right = rightTreesFrom position forest

walk :: Forest -> [Position]
walk forest = [(x, y) | x <- xs, y <- ys]
  where
    xs = [1 .. (length forest -2)]
    ys = [1 .. (length forest -2)]

topTreesFrom :: Position -> Forest -> String
topTreesFrom (x, y) forest = findTrees (x, y)
  where
    findTrees (x, y)
      | x == 0 = []
      | otherwise = forest !! (x - 1) !! y : findTrees (x - 1, y)

bottomTreesFrom :: Position -> Forest -> String
bottomTreesFrom (x, y) forest = findTrees (x, y)
  where
    findTrees (x, y)
      | x == (length forest - 1) = []
      | otherwise = forest !! (x + 1) !! y : findTrees (x + 1, y)

leftTreesFrom :: Position -> Forest -> String
leftTreesFrom (x, y) forest = findTrees (x, y)
  where
    findTrees (x, y)
      | y == 0 = []
      | otherwise = forest !! x !! (y - 1) : findTrees (x, y - 1)

rightTreesFrom :: Position -> Forest -> String
rightTreesFrom (x, y) forest = findTrees (x, y)
  where
    findTrees (x, y)
      | y == length forest - 1 = []
      | otherwise = forest !! x !! (y + 1) : findTrees (x, y + 1)
