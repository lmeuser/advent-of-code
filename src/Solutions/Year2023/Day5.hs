module Solutions.Year2023.Day5 where

import Control.Monad (void)
import Data.List.Split (chunksOf)
import Data.Range
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = (,) <$> seeds <* newline <* newline <*> sepBy mapping newline
  where seeds = string "seeds: " *> sepBy decimal (char ' ')
        mapping = mappingHeader *> sepEndBy mappingLine newline
        mappingHeader = void <$> many (lowerChar <|> char '-') <* string " map:" <* newline
        mappingLine = buildRangeOffset <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal
        buildRangeOffset dst src len = (src +=* (src + len), dst - src)

applyMappingPart seeds (src, offset) = map (fmap (+ offset)) (intersection seeds [src])

applyMapping seeds m = shifted `union` unshifted
  where shifted = concatMap (applyMappingPart seeds) m
        unshifted = difference seeds (map fst m)

solve f (seeds, mappings) = minimum . map lowerBound . foldl applyMapping (f seeds) $ mappings
  where lowerBound (SpanRange (Bound x Inclusive) _) = x
        lowerBound (SingletonRange x) = x

solve1 = solve (map SingletonRange)
solve2 = solve (map (\[a, b] -> a +=* (a + b)) . chunksOf 2)

solution = runSolution parser solve1 solve2
