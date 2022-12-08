module Day7 where

import Data.List.Split (splitOn)

type Name = String

type Size = Int

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show, Eq)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show, Eq)

type FSZipper = (FSItem, [FSCrumb])

solve :: [String] -> Int
solve xs = sum . map snd . dirsSmaller100000 $ fileSystem
  where
    (fileSystem, _) = performParsing xs

solvePartTwo :: [String] -> Int
solvePartTwo xs = minimum . filter (>= spaceNeededForUpdate) . map snd . dirs $ fileSystem
  where
    spaceNeededForUpdate = updateSize - sizeOfUnusedSpace
    sizeOfUnusedSpace = totalDiskSpace - greatestDirectory
    greatestDirectory = maximum . map snd . dirs $ fileSystem
    (fileSystem, _) = performParsing xs

totalDiskSpace :: Int
totalDiskSpace = 70000000

updateSize :: Int
updateSize = 30000000

dirsSmaller100000 :: FSItem -> [(String, Int)]
dirsSmaller100000 = filter (\(_, size) -> size <= 100000) . dirs

performParsing :: [String] -> FSZipper
performParsing xs = parse xs (Folder "Root" [], [])

parse :: [String] -> FSZipper -> FSZipper
parse [] current = fsRoot current
parse (x : xs) current@(Folder folderName items, bs) = go . splitOn " " $ x
  where
    go ["$", "cd", "/"] = parse xs (fsRoot current)
    go ["$", "cd", ".."] = parse xs (fsUp current)
    go ["$", "cd", dirName] = parse xs (attachFolder dirName current)
    go ["$", "ls"] = parse xs current
    go ["dir", dirName] = parse xs (Folder folderName (Folder dirName [] : items), bs)
    go [fileSize, fileName] = parse xs (Folder folderName (File fileName (read fileSize) : items), bs)
    go _ = error "Unsupported instruction"
parse _ _ = error "Unsupported instruction"

attachFolder :: Name -> FSZipper -> FSZipper
attachFolder name (Folder folderName items, bs) = (item, FSCrumb folderName ls rs : bs)
  where
    (ls, item : rs) = break (nameIs name) items
attachFolder _ _ = error "Could not attach folder"

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRoot :: FSZipper -> FSZipper
fsRoot root@(item, []) = root
fsRoot folder = fsRoot (fsUp folder)

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)
fsUp _ = error "Could not move out one level"

dirs :: FSItem -> [(String, Int)]
dirs folder@(Folder name items) = (name, dirSize folder) : concatMap dirs items
dirs _ = []

dirSize :: FSItem -> Int
dirSize (File _ size) = size
dirSize (Folder _ []) = 0
dirSize (Folder name (item : items)) = dirSize item + dirSize (Folder name items)
