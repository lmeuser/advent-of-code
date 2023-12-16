module Solutions.Year2023.Day13 where

import Data.List (transpose, findIndices)
import Data.List.Split (chunksOf)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser = sepBy pattern newline
  where pattern = sepEndBy line newline
        line = some (char '#' <|> char '.')

reflectsAt pattern n = zipWith (==) (reverse a) b
  where (a, b) = splitAt n pattern

test pattern = fmap (+1) . findIndices and $ map (reflectsAt pattern) [1..length pattern - 1]

numbersForPattern pattern = let (as, bs) = (test pattern, test (transpose pattern))
                            in map (* 100) as ++ bs

smudged pattern = map (chunksOf width) . variations . concat $ pattern
  where width = length . head $ pattern
        replace xs n = let (a, x:b) = splitAt n xs
                       in a ++ invert x:b
        invert '.' = '#'
        invert '#' = '.'
        variations line = map (replace line) [0..length line - 1]

solve = sum . map (head . numbersForPattern)

solve2 = sum . map smudgedNumber
  where smudgedNumber p = let n = head . numbersForPattern $ p
                          in head . concatMap (filter (\x -> x /= n && x /= 0) . numbersForPattern) . smudged $ p

solution = runSolution parser solve solve2
