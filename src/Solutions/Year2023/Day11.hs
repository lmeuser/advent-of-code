{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day11 where

import qualified Data.IntMap as M
import Data.List (findIndices, nub, sort)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser = process <$> sepBy line newline
  where line = many ((True <$ char '#') <|> (False <$ char '.'))
        process rs = let bounds = (length rs, length (head rs))
                         out = concat [map (row,) (findIndices id l) | (row, l) <- zip [0..] rs]
                     in (out, bounds)

expand size (xs, (maxR, maxC)) = map replace xs
  where (rs, cs) = unzip xs
        rowMap = M.fromList . buildMapping maxR . nub . sort $ rs
        colMap = M.fromList . buildMapping maxC . nub . sort $ cs
        buildMapping end xs = step [0..end] xs 0
          where step (i:is) xs@(x:xr) c
                  | i == x = (i, c):step is xr (c+1)
                  | otherwise = (i, c):step is xs (c+size)
                step (i:is) [] c = (i, c):step is [] (c+1)
                step [] [] _ = []
        replace (r, c) = (rowMap M.! r, colMap M.! c)

solve size = sum . map dist . pairs . expand size
  where dist ((a, b), (c, d)) = abs (a - c) + abs (b - d)
        pairs (x:xs) = map (x,) xs ++ pairs xs
        pairs [] = []

solution = runSolution parser (solve 2) (solve 1000000)
