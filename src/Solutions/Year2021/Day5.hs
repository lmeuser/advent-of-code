module Solutions.Year2021.Day5 where

import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)

--   let lines5 = map parseLine . lines $ input5

data Line = Line Int Int Int Int

parser = endBy line newline
  where line = Line <$> (decimal <* char ',') <*> (decimal <* string " -> ") <*> (decimal <* char ',') <*> decimal

isHorV (Line x1 y1 x2 y2) = (x1 == x2) || (y1 == y2)

pointsInLine (Line x1 y1 x2 y2)
  | x1 == x2 = pointsInVerticalLine x1 y1 y2
  | y1 == y2 = pointsInHorizontalLine y1 x1 x2
  | otherwise = pointsInDiagonalLine x1 y1 x2 y2
  where
    pointsInVerticalLine x y1 y2 = map (\y -> (x, y)) [y1'..y2']
      where y1' = min y1 y2
            y2' = max y1 y2

    pointsInHorizontalLine y x1 x2 = map (\x -> (x, y)) [x1'..x2']
      where x1' = min x1 x2
            x2' = max x1 x2
    
    pointsInDiagonalLine x1 y1 x2 y2 = map (\n -> (x1' + n, y1' + yDirection * n)) [0..xDiff]
      where (x1', y1') = min (x1, y1) (x2, y2)
            (x2', y2') = max (x1, y1) (x2, y2)
            xDiff = x2' - x1'
            yDirection = if y1' < y2' then 1 else -1

pointsInLines = concatMap pointsInLine


countMoreThanOnce ls = length . filter (>= 2) . map snd . helper p 1 $ ps
  where (p:ps) = sort . pointsInLines $ ls
        helper prev count (p:ps) = if p == prev
                                   then helper p (count + 1) ps
                                   else (prev, count):helper p 1 ps
        helper _ _ [] = []

solve1 = countMoreThanOnce . filter isHorV
solve2 = countMoreThanOnce

solution = runSolution parser solve1 solve2
