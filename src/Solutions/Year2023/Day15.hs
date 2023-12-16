module Solutions.Year2023.Day15 where

import Data.Array
import Data.Char (ord)
import Data.List (findIndex)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

data Operation = Set Int | Remove

encode (Set n) = '=':show n
encode Remove = "-"

parser = sepBy part (char ',')
  where part = (,) <$> some lowerChar <*> operation
        operation = (Set <$> (char '=' *> decimal)) <|> (Remove <$ char '-' )

hash = foldl (\a c -> ((a + ord c) * 17) `mod` 256) 0

solve1 = sum . map (hash . \(a, b) -> a ++ encode b)

solve2 = value . foldl step (listArray (0, 255) (repeat []))
  where step boxes (label, op) = let index = hash label
                                 in boxes // [(index, updateBox (boxes ! index) label op)]
        updateBox box label op = case op of
                                   Remove -> filter ((/= label) . fst) box
                                   Set n -> case findIndex ((== label) . fst) box of
                                              Just i -> take i box ++ (label, n):drop (i+1) box
                                              Nothing -> box ++ [(label, n)]
        value = sum . zipWith (*) [1..256] . map lensValue . elems
        lensValue = sum . zipWith (*) [1..] . map snd

solution = runSolution parser solve1 solve2
