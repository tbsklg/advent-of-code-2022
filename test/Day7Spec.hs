module Day7Spec where

import Day7 (FSCrumb (..), FSItem (..), FSZipper (..), parse, solve)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "no space left on device" $ do
    it "should calculate the sum of their total sizes for directories of at most 100000" $ do
      solve ["$ cd /", "$ ls", "dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a", "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst", "$ cd e", "$ ls", "584 i", "$ cd ..", "$ cd ..", "$ cd d", "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k"]
        `shouldBe` 95437

    it "should create a zipper file system" $ do
      parse ["$ cd /"] (Folder "Root" [], [])
        `shouldBe` (Folder "Root" [], [])
      parse ["$ cd /", "$ ls", "dir a"] (Folder "Root" [], [])
        `shouldBe` (Folder "Root" [Folder "a" []], [])
      parse ["$ cd /", "$ ls", "dir a", "815 b.txt"] (Folder "Root" [], [])
        `shouldBe` (Folder "Root" [File "b.txt" 0815, Folder "a" []], [])

      -- focus on subdirectory
      parse ["$ cd /", "$ ls", "dir a", "815 root.txt", "$ cd a", "$ ls", "816 a.txt"] (Folder "Root" [], [])
        `shouldBe` (Folder "a" [File "a.txt" 816], [FSCrumb "Root" [File "root.txt" 815] []])

      parse ["$ cd /", "$ ls", "dir a", "815 root.txt", "$ cd a", "$ ls", "816 a.txt", "$ cd .."] (Folder "Root" [], [])
        `shouldBe` (Folder "Root" [File "root.txt" 815, Folder "a" [File "a.txt" 816]], [])

      parse ["$ cd /", "$ ls", "dir a", "14848514 b.txt", "8504156 c.dat", "dir d", "$ cd a", "$ ls", "dir e", "29116 f", "2557 g", "62596 h.lst", "$ cd e", "$ ls", "584 i", "$ cd ..", "$ cd ..", "$ cd d", "$ ls", "4060174 j", "8033020 d.log", "5626152 d.ext", "7214296 k"] (Folder "Root" [], [])
        `shouldBe` (Folder "d" [File "k" 7214296, File "d.ext" 5626152, File "d.log" 8033020, File "j" 4060174], [FSCrumb "Root" [] [File "c.dat" 8504156, File "b.txt" 14848514, Folder "a" [File "h.lst" 62596, File "g" 2557, File "f" 29116, Folder "e" [File "i" 584]]]])
