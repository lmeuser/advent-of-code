module Days.Day6 where

import Data.List (elemIndex, group, sort, tails)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)


parser = many letterChar

solveFor n = (+n) . fromJust . (True `elemIndex`) . map sop . tails
  where sop = (== n) . length . group . sort . take n

solve1 = solveFor 4
solve2 = solveFor 14

solution = runSolution parser solve1 solve2
