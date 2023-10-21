module Solutions.Year2022.Day4 where
import Shared ( runSolution, Parser )
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer ( decimal )


parser = sepBy line newline
  where line = (,,,) <$> (decimal <* char '-') <*> (decimal <* char ',') <*> (decimal <* char '-') <*> decimal

countPairs f = length . filter f

solve1 = countPairs (\(a, b, c, d) -> (a >= c && b <= d) || (a <= c && b >= d))
solve2 = countPairs (\(a, b, c, d) -> c <= b && a <= d)

solution = runSolution parser solve1 solve2
