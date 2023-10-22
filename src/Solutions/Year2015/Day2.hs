module Solutions.Year2015.Day2 where

import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Paper = Paper Int Int Int

parser :: Parser [Paper]
parser = sepBy paper newline
    where paper = Paper <$> decimal <* char 'x' <*> decimal <* char 'x' <*> decimal

solve1 :: [Paper] -> Int
solve1 = sum . map value
    where value (Paper l w h) = 2 * (l*w + w*h + h*l) + (product . take 2 . sort $ [l, w, h])

solve2 :: [Paper] -> Int
solve2 = sum . map value
    where value (Paper l w h) = w * l * h + 2 * (sum . take 2 . sort $ [l, w, h])

solution :: SolutionRunner
solution = runSolution parser solve1 solve2
