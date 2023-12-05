{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Solutions.Year2023.Day5 where

import Data.List (sortOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared
import Data.List.Split (chunksOf)


newtype Mapping = Mapping [(Int, Int, Int)] deriving Show

buildMapping = Mapping . sortOn snd3
  where snd3 (_, x, _) = x

parser = (,) <$> seeds <* doubleNew <*> sepBy mapping newline
  where seeds = string "seeds: " *> sepBy decimal (char ' ')
        doubleNew = () <$ (newline *> newline)
        mapping = buildMapping <$> (mappingHeader *> sepEndBy mappingLine newline)
        mappingHeader = (,) <$> (many lowerChar <* string "-to-") <*> (many lowerChar <* string " map:") <* newline
        mappingLine = (,,) <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal

applyMapping n (Mapping m) = helper n m
  where helper n [] = n
        helper n ((dst, src, len):xs)
          | n >= src && n < src + len = n - src + dst
          | n > src = helper n xs
          | otherwise = n

applyMappings mappings seed = foldl applyMapping seed mappings

solve seeds mappings = minimum . map (applyMappings mappings) $ seeds

solve1 (seeds, mappings) = solve seeds mappings

solve2 (seeds, mappings) = solve seeds' mappings
  where seeds' = concatMap (\[a, b] -> [a..a+b-1]) . chunksOf 2 $ seeds

solution = runSolution parser solve1 solve2
