module Solutions.Year2019.Day2 where

import Data.List (find)
import Data.Maybe (fromJust)

import Solutions.Year2019.Intcode
import Shared

solve1 = head . fst . runIntcode [] . flip updateMemory [(1, 12), (2, 2)]

solve2 input = sol . fromJust . find ((== 19690720) . calc) $ [(a, b) | a <- [0..99], b <- [0..99]]
  where calc (a, b) = head . fst . runIntcode [] . updateMemory input $ [(1, a), (2, b)]
        sol (a, b) = a * 100 + b

solution = runSolution intcodeParser solve1 solve2
