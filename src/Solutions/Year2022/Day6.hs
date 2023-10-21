module Solutions.Year2022.Day6 where

import Data.List (findIndex, nub, tails)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)


parser = many letterChar

solveFor n = (+n) . fromJust . findIndex (match . take n) . tails
  where match xs = nub xs == xs

solve1 = solveFor 4
solve2 = solveFor 14

solution = runSolution parser solve1 solve2
