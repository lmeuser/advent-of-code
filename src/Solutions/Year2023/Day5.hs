module Solutions.Year2023.Day5 where

import Control.Monad (void)
import Data.List (sortOn, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (maybeToList, isNothing)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = (,) <$> seeds <* doubleNew <*> sepBy mapping newline
  where seeds = string "seeds: " *> sepBy decimal (char ' ')
        doubleNew = void (newline *> newline)
        mapping = sortOn (fst . fst) <$> (mappingHeader *> sepEndBy mappingLine newline)
        mappingHeader = void <$> many (lowerChar <|> char '-') <* string " map:" <* newline
        mappingLine = buildRangeLen <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal
        buildRangeLen dst src len = ((src, src + len - 1), dst)

overlap r1@(s1, e1) (s2, e2)
  | e1 < s2 = (Just r1, Nothing, Nothing)
  | s1 > e2 = (Nothing, Nothing, Just r1)
  | otherwise = let pre = if s1 < s2
                          then Just (s1, s2 - 1)
                          else Nothing
                    post = if e1 > e2
                           then Just (e2 + 1, e1)
                           else Nothing
                    match = Just (max s1 s2, min e1 e2)
                in (pre, match, post)

applyMapping seeds = sort . helper seeds
  where helper ss [] = ss
        helper ss@(s:sr) ms@((src@(start, _), dst):mr)
          | isNothing pre && isNothing match = helper ss mr
          | otherwise = pre' ++ match' ++ helper left ms
          where (pre, match, post) = overlap s src
                dist = dst - start
                move (a, b) =  (a + dist, b + dist)
                pre' = maybeToList pre
                match' = map move . maybeToList $ match
                left = maybeToList post ++ sr
        helper [] _ = []

applyMappings mappings seeds = foldl applyMapping seeds mappings

solve f (seeds, mappings) = minimum . map fst . applyMappings mappings . f $ seeds

solve1 = solve (map (\x -> (x, x)) . sort)

solve2 = solve (map (\[a, b] -> (a, a + b - 1)) . sort . chunksOf 2)

solution = runSolution parser solve1 solve2
