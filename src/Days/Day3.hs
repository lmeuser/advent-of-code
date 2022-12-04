module Days.Day3 where

import Data.Char (ord)
import Shared (runSolution)

common [x] = x
common (x:xs) = filter (`elem` (common xs)) x

prio c
  | n > 90    = n - 96
  | otherwise = n - 38
  where n = ord c


solution1 input = sum . map (prio . head . common . \(a, b) -> [a, b]) $ rucksacks
  where rucksacks = map (\x -> splitAt (length x `div` 2) x) . lines $ input


splitGroups [] = []
splitGroups xs = as:splitGroups bs
  where (as, bs) = splitAt 3 xs

solution2 = sum . map (prio . head . common) . splitGroups . lines

solution = runSolution solution1 solution2