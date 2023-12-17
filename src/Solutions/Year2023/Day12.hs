module Solutions.Year2023.Day12 where

import Control.Parallel.Strategies
import Data.List (intercalate)
import Data.MemoTrie (memo2)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = sepBy line newline
  where line = (,) <$> some spring <* hspace <*> sepBy decimal (char ',')
        spring = char '.' <|> char '#' <|> char '?'

expand n (a, b) = (expand1 n "?" a, expand1 n [] b)
  where expand1 n x = intercalate x . replicate n

calc = memo2 calc'

calc' [] ls = if null ls then 1 else 0
calc' ss [] = if '#' `notElem` ss then 1 else 0
calc' ss (l:ls) = let ss' = dropWhile (== '.') ss
                      (start, rest) = splitAt l ss'
                      p1 = if length start == l && '.' `notElem` start && (null rest || head rest /= '#')
                           then calc (drop 1 rest) ls
                           else 0
                      p2 = if not (null ss') && head ss' /= '#'
                           then calc (tail ss') (l:ls)
                           else 0
                  in p1 + p2

solve :: Int -> [(String, [Int])] -> Int
solve n = sum . parMap rdeepseq (uncurry calc . expand n)

solution = runSolution parser (solve 1) (solve 5)
