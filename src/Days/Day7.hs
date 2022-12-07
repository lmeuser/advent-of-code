module Days.Day7 where

import Data.List (intercalate, inits)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)


data Command = Cd String | Ls [LsResult]
data LsResult = File Int String | Directory String

parser = M.elems . buildSizeMap M.empty "/" <$> many (cdCommand <|> lsCommand)
  where cdCommand = Cd <$> (string "$ cd " *> many printChar <* newline)
        lsCommand = Ls <$> (string "$ ls\n" *> sepEndBy (dirResult <|> fileResult) newline)
        dirResult = Directory <$> (string "dir " *> many printChar)
        fileResult = File <$> (decimal <* hspace) <*> many printChar

        buildSizeMap m _ [] = m
        buildSizeMap m dir (Cd path:cs) = buildSizeMap m path' cs
          where path' = case path of
                          "/" -> "/"
                          ".." -> intercalate "/" . init . splitOn "/" $ dir
                          into -> dir ++ '/':into
        buildSizeMap m dir (Ls results:cs) = buildSizeMap (foldl step m results) dir cs
          where step m (File s _) = foldl (\m' d -> M.insertWith (+) d s m') m (ancestorDirs dir)
                step m (Directory _) = m
                ancestorDirs = map (intercalate "/") . drop 2 . inits . splitOn "/"

solve1 = sum . filter (<= 100000)
solve2 cs = minimum . filter (> head cs - 40000000) $ cs

solution = runSolution parser solve1 solve2
