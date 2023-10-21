module Solutions.Year2021.Day6 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)


parser = convert (replicate 9 0) <$> sepBy decimal (char ',')
  where convert result [] = result
        convert result (x:xs) = convert (take x result ++ [(result !! x) + 1] ++ drop (x + 1) result) xs

solve n = sum . (!! n) . iterate fishGeneration
  where fishGeneration [a, b, c, d, e, f, g, h, i] = [b, c, d, e, f, g, h + a, i, a]

solve1 = solve 80
solve2 = solve 256

solution = runSolution parser solve1 solve2
