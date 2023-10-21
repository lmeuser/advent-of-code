module Solutions.Year2021.Day10 where

import Data.List (sort)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared (runSolution)

parser = sepBy (many (satisfy (`elem` "()[]{}<>"))) newline

data Result = Correct | Mismatch Char | Missing String

evalLine = helper []
  where helper stack [] = if null stack then Correct else Missing stack
        helper stack (x:xs)
          | x `elem` "([{<" = helper (x:stack) xs
          | otherwise = if [head stack, x] `elem` ["()", "[]", "{}", "<>"]
                        then helper (tail stack) xs
                        else Mismatch x

solve1 = sum . map (points . evalLine)
  where points (Mismatch ')') = 3
        points (Mismatch ']') = 57
        points (Mismatch '}') = 1197
        points (Mismatch '>') = 25137
        points _ = 0

solve2 = median . filter (/= 0) . map (calcPoints . evalLine)
  where calcPoints (Missing s) = foldl (\acc r -> acc * 5 + points r) 0 s
        calcPoints _ = 0
        points '(' = 1
        points '[' = 2
        points '{' = 3
        points '<' = 4
        median xs = sort xs !! ((length xs - 1) `div` 2)

solution = runSolution parser solve1 solve2
