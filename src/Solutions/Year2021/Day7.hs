module Solutions.Year2021.Day7 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)

parser = sepBy decimal (char ',')

solve cost crabs = minimum . map totalFuelCost $ [0..maximum crabs]
  where totalFuelCost pos = sum . map (cost pos) $ crabs

distance a b = abs (a - b)

solve1 = solve distance
solve2 = solve cost
  where cost p c = let x = distance p c in (x + 1) * x `div` 2

solution = runSolution parser solve1 solve2
