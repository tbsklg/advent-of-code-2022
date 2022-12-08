module Day7 where

import Data.List.Split (splitOn)

type Name = String

type Size = Int

data FSItem = File Name Size | Folder Name [FSItem] deriving (Show, Eq)

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show, Eq)

type FSZipper = (FSItem, [FSCrumb])

solve :: [String] -> Int
solve xs = 0

parse :: [String] -> FSZipper -> FSZipper
parse [] current = current
parse (x : xs) current@(Folder folderName items, bs) = go . splitOn " " $ x
  where
    go ["$", "cd", "/"] = parse xs (Folder "Root" [], [])
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

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs : bs) = (Folder name (ls ++ [item] ++ rs), bs)
fsUp _ = error ""
