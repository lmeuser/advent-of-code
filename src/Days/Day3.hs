module Days.Day3 where

import Data.Char ( ord )
import Data.List.Split ( chunksOf )
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared ( runSolution )
import Data.List ( intersect )


parser = endBy (many letterChar) newline

prio c
  | n > 90    = n - 96
  | otherwise = n - 38
  where n = ord c

solve1 = sum . map (prio . head . uncurry intersect . split)
  where split xs = splitAt (length xs `div` 2) xs

solve2 = sum . map (prio . head . foldr1 intersect) . chunksOf 3

solution = runSolution parser solve1 solve2
