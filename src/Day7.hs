module Day7 where

import Data.List.Split (splitOn)

type Name = String

type Size = Int

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show, Eq)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show, Eq)

type FSZipper = (FSItem, [FSCrumb])

solve :: [String] -> Int
solve xs = sum . map snd . dirsAtMost100000 $ fileSystem
  where
    (fileSystem, _) = parse xs (Folder "Root" [], [])

dirsAtMost100000 :: FSItem -> [(String, Int)]
dirsAtMost100000 = filter (\(_, size) -> size <= 100000) . dirs

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
    go _ = error "fdsa"
parse _ _ = error "fdsa"

attachFolder :: Name -> FSZipper -> FSZipper
attachFolder name (Folder folderName items, bs) = (item, FSCrumb folderName ls rs : bs)
  where
    (ls, item : rs) = break (nameIs name) items
attachFolder _ _ = error ""

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName

fsRoot :: FSZipper -> FSZipper
fsRoot root@(item, []) = root
fsRoot folder = fsRoot (fsUp folder)

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)
fsUp _ = error ""

dirs :: FSItem -> [(String, Int)]
dirs folder@(Folder name items) = (name, dirSize folder) : concatMap dirs items
dirs _ = []

dirSize :: FSItem -> Int
dirSize (File _ size) = size
dirSize (Folder _ []) = 0
dirSize (Folder name (item : items)) = dirSize item + dirSize (Folder name items)
