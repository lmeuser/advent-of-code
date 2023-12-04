module Solutions.Year2019.Day1 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = sepBy decimal newline

fuel = subtract 2 . (`div` 3)

fuel' n = initial + sum (remaining initial)
  where initial = fuel n
        remaining m = let value = fuel m
                      in if value <= 0
                         then []
                         else value:remaining value

solve1 = sum . map fuel

solve2 = sum . map fuel'

solution = runSolution parser solve1 solve2