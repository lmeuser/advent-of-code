{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use void" #-}
module Solutions.Year2023.Day5 where

import Data.List (sortOn)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared


newtype Mapping = Mapping [(Int, Int, Int)]

-- buildMapping = Mapping . sortOn fst3
--   where fst3 (x, _, _) = x

-- applyMapping (Mapping m) n = helper n m
--   where helper n [] = n
--         helper n ((dst, src, len):xs) = if n >= src
--                                         then if n < src + len
--                                              then n - src + dst
--                                              else n
--                                         else 

parser = (,) <$> seeds <* doubleNew <*> sepBy mapping newline
  where seeds = string "seeds: " *> sepBy decimal (char ' ')
        doubleNew = () <$ (newline *> newline)
        mapping = mappingHeader *> sepEndBy mappingLine newline
        mappingHeader = (,) <$> (many lowerChar <* string "-to-") <*> (many lowerChar <* string " map:") <* newline
        mappingLine = (,,) <$> (decimal <* char ' ') <*> (decimal <* char ' ') <*> decimal

solution = runSolution parser (const ()) id
