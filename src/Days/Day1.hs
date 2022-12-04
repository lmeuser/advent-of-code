module Days.Day1 where

import Data.List (sortBy)
import Data.List.Split (splitOn)

import Shared (runSolution)


elves :: String -> [[Integer]]
elves = map (map read . lines) . splitOn "\n\n"

elfTotals :: String -> [Integer]
elfTotals = map sum . elves

elfData = sortBy (flip compare) . elfTotals

solve1 = (!! 0) . elfData
solve2 = sum . take 3 . elfData

solution = runSolution solve1 solve2
