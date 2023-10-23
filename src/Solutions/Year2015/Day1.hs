module Solutions.Year2015.Day1 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Shared

parser :: Parser String
parser = many (char '(' <|> char ')')

next :: Num a => Char -> a -> a
next '(' = (+ 1)
next ')' = subtract 1

solve1 :: String -> Int
solve1 = foldl (flip next) 0

solve2 :: String -> Int
solve2 = step 0 0
    where step pos i (x:xs) = if pos == (-1)
                              then i
                              else step (next x pos) (i + 1) xs

solution :: SolutionRunner
solution = runSolution parser solve1 solve2
