{-# LANGUAGE TupleSections #-}

module Solutions.Year2021.Day14 where

import Data.List (sort)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)

parser = (,) <$> (init <$> many upperChar) <* space <*> (M.fromList <$> sepBy rules newline)
  where init s = M.fromListWith (+) . zipWith (curry (,1)) ('-':s) $ s
        rules = rule <$> upperChar <*> upperChar <* string " -> " <*> upperChar
        rule a b c = ((a, b), [(a, c), (c, b)])

applyRules r = M.fromListWith (+) . concatMap nextPairs . M.toList
  where nextPairs p = case M.lookup (fst p) r of
          Nothing -> [p]
          Just ps -> map (, snd p) ps

solveFor n (s, r) = (\l -> last l - head l) . sort . map snd . M.toList . M.mapKeysWith (+) snd . (!! n) . iterate (applyRules r) $ s

solution = runSolution parser (solveFor 10) (solveFor 40)
