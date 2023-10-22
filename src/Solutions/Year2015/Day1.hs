module Solutions.Year2015.Day1 where

import Text.Megaparsec
import Text.Megaparsec.Char

import Shared (runSolution, SolutionRunner, Parser)

parser :: Parser String
parser = many (char '(' <|> char ')')

next :: Num a => Char -> a -> a
next '(' = (+ 1)
next ')' = subtract 1

solve1 :: Int -> String -> Int
solve1 = foldl (flip next)

solve2 :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> [Char] -> t2
solve2 pos i (x:xs) = if pos == (-1)
                      then i
                      else solve2 (next x pos) (i + 1) xs

solution :: SolutionRunner
solution = runSolution parser (solve1 0) (solve2 0 0)
