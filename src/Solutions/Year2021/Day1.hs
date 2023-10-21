module Solutions.Year2021.Day1 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared (runSolution)


parser = endBy decimal newline

compared [] = []
compared [_] = []
compared (x:y:xs) = compare x y:compared (y:xs)

solve1 = length . filter (== LT) . compared

slidingWindows n xs@(x:xr) 
  | length window < n = []
  | otherwise = window:slidingWindows n xr
  where window = take n xs

solve2 = solve1 . map sum . slidingWindows 3

solution = runSolution parser solve1 solve2
