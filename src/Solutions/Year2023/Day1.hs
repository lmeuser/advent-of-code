module Solutions.Year2023.Day1 where

import Data.Char (digitToInt, isDigit)
import Data.List (isPrefixOf)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser = sepBy (many alphaNumChar) newline

getNumber xs = head xs * 10 + last xs

findDigits xs@(x:xr) = case digit of
  Just n -> n:findDigits xr
  Nothing -> findDigits xr
  where digit
          | "one" `isPrefixOf` xs = Just 1
          | "two" `isPrefixOf` xs = Just 2
          | "three" `isPrefixOf` xs = Just 3
          | "four" `isPrefixOf` xs = Just 4
          | "five" `isPrefixOf` xs = Just 5
          | "six" `isPrefixOf` xs = Just 6
          | "seven" `isPrefixOf` xs = Just 7
          | "eight" `isPrefixOf` xs = Just 8
          | "nine" `isPrefixOf` xs = Just 9
          | isDigit x = Just (digitToInt x)
          | otherwise = Nothing
findDigits [] = []

solve1 = sum . map (getNumber . map digitToInt . filter isDigit)
solve2 = sum . map (getNumber . findDigits)

solution = runSolution parser solve1 solve2
