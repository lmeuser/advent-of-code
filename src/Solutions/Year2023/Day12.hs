{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day12 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared
import Data.List (group)

options 0 = [""]
options n = let r = options (n - 1)
            in map ('#':) r ++ map ('.':) r

replace [] _ = []
replace s [] = s
replace (s:ss) os@(o:or)
  | s == '?' = o:replace ss or
  | otherwise = s:replace ss os

solve123 (s, sol) = length . filter (== sol) $ ls
  where os :: [[Char]]
        os = options (length . filter (== '?') $ s)
        fils :: [[Char]]
        fils = map (replace s) os
        brokenParts :: [[[Char]]]
        brokenParts = map (filter ((== '#') . head) . group) $ fils
        ls :: [[Int]]
        ls = map (map length) brokenParts


-- length . filter (== [3,2,1]) $ map (map length . filter ((== '#') . head) . group) (map (replace "?###????????") (options 12))

solve1 = sum . map solve123

parser = sepBy line newline
  where line = (,) <$> some spring <* hspace <*> sepBy decimal (char ',')
        spring = char '.' <|> char '#' <|> char '?'

solution = runSolution parser (const ()) solve1
