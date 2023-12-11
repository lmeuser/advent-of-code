{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day11 where

import Text.Megaparsec
import Text.Megaparsec.Char

import Shared
import Data.List (findIndices)

parser :: Parser ([(Int, Int)], (Int, Int))
parser = process <$> sepBy line newline
  where line = many ((True <$ char '#') <|> (False <$ char '.'))
        process rs = let bounds = (length rs, length (head rs))
                         out = concat [map (row,) (findIndices id l) | (row, l) <- zip [0..] rs]
                     in (out, bounds)

solution = runSolution eof (const ()) id
