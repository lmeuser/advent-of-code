{-# LANGUAGE TupleSections #-}
module Solutions.Year2021.Day15 where

import Data.Char (digitToInt)
import qualified Data.Heap as H
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution, listsToMap, Parser)
import Solutions.Year2021.Day11 (neighbors)


parser :: Parser (M.Map (Int, Int) Int)
parser = listsToMap <$> sepBy (many (digitToInt <$> digitChar)) newline

solve base = step M.empty initialQueue
  where initialQueue = H.fromList ((0, (0, 0)):map (maxBound - 10, ) (M.keys base))
        goal = fst (M.findMax base)
        step :: M.Map (Int, Int) Int -> H.MinPrioHeap Int (Int, Int) -> Int
        step done queue = case H.view queue of
          Nothing -> undefined
          Just ((val, pos), queue')
            | pos == goal -> val
            | pos `M.member` done -> step done queue'
            | otherwise -> let queue'' = foldl (flip H.insert) queue' . map (\npos -> ((base ! npos) + val, npos)) $ newNeighbors
                           in step (M.insert pos val done) queue''
            where newNeighbors = filter (\p -> p `M.member` base && not (p `M.member` done)) neighbors
                  neighbors = let (r, c) = pos in [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

cloneMap m = M.fromList [((r, c), valueOf r c) | r <- [0..rows'], c <- [0..cols']]
  where (rows, cols) = fst (M.findMax m)
        (rows', cols') = ((rows + 1) * 5 - 1, (cols + 1) * 5 - 1)
        valueOf row col = let (rmod, ri) = row `divMod` (rows + 1)
                              (cmod, ci) = col `divMod` (cols + 1)
                          in ((((m ! (ri, ci)) + (rmod + cmod)) - 1) `mod` 9) + 1

solution = runSolution parser solve (solve . cloneMap)
