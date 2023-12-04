module Solutions.Year2023.Day4 where

import Data.List (intersect)
import Data.Maybe (isJust, fromJust)
import qualified Data.Map as M
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Card = Card [Int] [Int]

parser = sepBy card newline
  where card = Card <$> (header *> numberList) <*> (separator *> numberList)
        header = string "Card" *> space1 *> decimal *> char ':' *> space1
        separator = string "|" *> space1
        numberList = sepEndBy decimal (some (char ' '))

matching (Card win my) = win `intersect` my

cardValue c = if null common
              then 0
              else 2 ^ (length common - 1)
  where common = matching c

solve1 = sum . map cardValue

solve2 xs = helper (zip [1..] (map (length . matching) xs)) M.empty
  where helper _ m | M.size m == size = sum m
        helper (x@(index, matches):xs') m = if canCalc
                                            then helper xs' (M.insert index solution m)
                                            else helper (xs' ++ [x]) m
          where indices = take matches . map (+ index) $ [1..]
                values = map (m M.!?) indices
                canCalc = all isJust values
                solution = (1+) . sum . map fromJust $ values
        size = length xs

solution = runSolution parser solve1 solve2
