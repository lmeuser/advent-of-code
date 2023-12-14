module Solutions.Year2023.Day14 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import qualified Data.Map as M

import Shared
import Data.List (transpose, intercalate, sort, iterate')
import Data.List.Split (splitOn)

parser = sepBy line newline
  where line = some (char 'O' <|> char '#' <|> char '.')

move = map move'
  where move' = intercalate "#" . map sort . splitOn "#"

load = sum . map load'
  where load' = sum . map fst . filter ((== 'O') . snd) . zip [1..]

solve = load . move . rotate

rotate = map reverse . transpose

spin1 = move . rotate

-- solve2 x = (x, rotate x, rotate (rotate x), rotate (rotate (rotate x)), rotate (rotate (rotate (rotate x))))
spinCycle x = iterate' spin1 x !! 4

-- solve2 x = load (iterate spinCycle x !! 1000000000)

foo x = step ( x) 0 M.empty
  where step x 1000000000 _ = load . rotate $ x
        step x n m = case m M.!? x of
                       Just n'      -> let len = n - n'
                                           bar = (1000000000 - n) `mod` len
                                       in load . rotate $ (iterate' spinCycle x !! bar)
                       Nothing -> step (spinCycle x) (n + 1) (M.insert x n m)


foo' x = map (load . rotate) . take 20 $ iterate spinCycle x

-- solve2 x = load (iterate' spinCycle x !! 100000)

solution = runSolution parser (load . rotate) foo
