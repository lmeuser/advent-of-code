module Solutions.Year2023.Day9 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = sepBy (sepBy (signed hspace decimal) (char ' ')) newline

extend xs@(x:xr)
  | all (== 0) xs = 0:xs
  | otherwise = let l = x:zipWith (+) l next
                    next = extend (zipWith (-) xr xs)
                in l

solve1 = sum . map (last . extend)
solve2 = solve1 . map reverse

solution = runSolution parser solve1 solve2
