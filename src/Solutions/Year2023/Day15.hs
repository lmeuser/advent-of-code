module Solutions.Year2023.Day15 where

import Data.Array
import Data.Char (ord)

import Text.Megaparsec
import Text.Megaparsec.Char

import Shared
import Text.Megaparsec.Char.Lexer (decimal)
import Data.List (findIndex)

data Operation = Set Int | Remove

encode (Set n) = '=':show n
encode Remove = "-"

parser = sepBy part (char ',')
  where part = (,) <$> some lowerChar <*> operation
        operation = (Set <$> (char '=' *> decimal)) <|> (Remove <$ char '-' )

hash = foldl (\a c -> ((a + ord c) * 17) `mod` 256) 0

solve1 = sum . map (hash . \(a, b) -> a ++ encode b)

solve2 = value . foldl step boxes
  where boxes = listArray (0, 255) (repeat [])
        step boxes (label, op) = let index = hash label
                                     box = boxes ! index
                                     box' = updateBox box label op
                                 in boxes // [(index, box')]
        updateBox box label op = case op of
                                   Remove -> filter ((/= label) . fst) box
                                   Set n -> case findIndex ((== label) . fst) box of
                                              Just i -> take i box ++ (label, n):drop (i+1) box
                                              Nothing -> box ++ [(label, n)]
        value = sum . zipWith (*) [1..256] . map foo . elems
        foo = sum . zipWith (*) [1..] . map snd

solution = runSolution parser solve1 solve2
