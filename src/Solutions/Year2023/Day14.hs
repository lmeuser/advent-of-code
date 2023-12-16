module Solutions.Year2023.Day14 where

import Data.List (transpose, intercalate, sort, iterate')
import Data.List.Split (splitOn)
import Text.Megaparsec
import Text.Megaparsec.Char

import qualified Data.Map as M

import Shared


parser = sepBy line newline
  where line = some (char 'O' <|> char '#' <|> char '.')

load = sum . map (sum . map fst . filter ((== 'O') . snd) . zip [1..])

rotate = map reverse . transpose

spin1 = map (intercalate "#" . map sort . splitOn "#") . rotate

spinCycle x = iterate' spin1 x !! 4

solve1 = load . spin1

solve2 x = step x 0 M.empty
  where step x n m = case m M.!? x of
                       Just n' -> let len = n - n'
                                      remaining = (1000000000 - n) `mod` len
                                  in load . rotate $ (iterate' spinCycle x !! remaining)
                       Nothing -> step (spinCycle x) (n + 1) (M.insert x n m)

solution = runSolution parser solve1 solve2
