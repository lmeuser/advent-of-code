module Solutions.Year2015.Day5 where

import Data.List.Split (divvy)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser :: Parser [String]
parser = sepBy (many lowerChar) newline

solve1 :: [String] -> Int
solve1 = length . filter nice
    where nice input = vowels input >= 3 && hasDouble input && noBad input
          vowels = length . filter (`elem` "aeiou")
          hasDouble (a:b:xs) = a == b || hasDouble (b:xs)
          hasDouble _ = False
          noBad (a:b:xs) = [a, b] `notElem` ["ab", "cd", "pq", "xy"] && noBad (b:xs)
          noBad _ = True

solve2 :: [String] -> Int
solve2 = length . filter nice
    where nice input = doublePair input && sepRepeat input
          doublePair (a:b:rest) = let chunks = divvy 2 1 rest
                                  in [a, b] `elem` chunks || doublePair (b:rest)
          doublePair _ = False
          sepRepeat (a:b:c:xs) = a == c || sepRepeat (b:c:xs)
          sepRepeat _ = False

solution :: SolutionRunner
solution = runSolution parser solve1 solve2
