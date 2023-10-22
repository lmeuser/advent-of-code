module Solutions.Year2015.Day1 where

import Text.Megaparsec
import Text.Megaparsec.Char

import Shared ( runSolution )

parser = many (char '(' <|> char ')')

next '(' = (+ 1)
next ')' = subtract 1

solve1 :: Int -> String -> Int
solve1 pos [] = pos
solve1 pos (x:xs) = solve1 (next x pos) xs

solve2 pos i (x:xs) = if pos == (-1)
                      then i
                      else solve2 (next x pos) (i + 1) xs

solution = runSolution parser (solve1 0) (solve2 0 0)
