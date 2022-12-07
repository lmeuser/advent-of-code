module Days.Day7 where

import Data.List (intercalate)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Data.Map ((!))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)


data Command = CdCommand String | LsCommand [LsResult]
data LsResult = FileResult String Int | DirectoryResult String

parser = many command
  where command = cdCommand <|> lsCommand
        cdCommand = CdCommand <$> (string "$ cd " *> many printChar <* newline)
        lsCommand = LsCommand <$> (string "$ ls\n" *> sepEndBy lsResult newline)
        lsResult = dirResult <|> fileResult
        dirResult = DirectoryResult <$> (string "dir " *> many printChar)
        fileResult = flip FileResult <$> (decimal <* hspace) <*> many printChar


buildFileMap m _ [] = m
buildFileMap m dir (x:xs) = case x of
  CdCommand "/" -> buildFileMap m "/" xs
  CdCommand ".." -> buildFileMap m (intercalate "/" . init . splitOn "/" $ dir) xs
  CdCommand into -> buildFileMap m (dir ++ '/': into) xs
  LsCommand results -> buildFileMap (foldl insertResult m results) dir xs
  where insertResult om result = M.insertWith (++) dir [result] om

data FileTree = File String Int | Directory String Int [FileTree]

size (File _ size) = size
size (Directory _ size _) = size

buildFileTree :: M.Map String [LsResult] -> String -> FileTree
buildFileTree m dir = let subtree = map buildSubTree (m ! dir)
                          treeSize = sum . map size $ subtree
                      in Directory dir treeSize subtree
  where buildSubTree (FileResult name size) = File name size
        buildSubTree (DirectoryResult name) = buildFileTree m (dir ++ '/':name)

allDirectories (File _ _) = []
allDirectories d@(Directory _ _ tree) = d:concatMap allDirectories tree

allDirSizesFromInput cs = map size . allDirectories $ buildFileTree (buildFileMap M.empty "/" cs) "/"

solve1 = sum . filter (<= 100000) . allDirSizesFromInput

solve2 cs = minimum . filter (>neededSpace) $ dirs
  where dirs = allDirSizesFromInput cs
        neededSpace = head dirs - 40000000

solution = runSolution parser solve1 solve2
