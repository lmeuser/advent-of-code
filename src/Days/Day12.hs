{-# LANGUAGE TupleSections #-}

module Days.Day12 where

import qualified Data.Heap as H
import Data.List (find)
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (buildMapList, runSolution, Parser)

parser :: Parser (M.Map (Int, Int) Char, (Int, Int), (Int, Int))
parser = process . buildMapList <$> sepBy (many letterChar) newline
  where process ml = (mm', start, end)
          where findChar c = fst . fromJust . find ((== c) . snd) $ ml
                start = findChar 'S'
                end = findChar 'E'
                mm = M.fromList ml
                mm' = M.insert start 'a' (M.insert end 'z' mm)

solve mkQueue (heightMap, start, end) = step M.empty (H.fromList (mkQueue heightMap start end))
  where step :: M.Map (Int, Int) Int -> H.MinPrioHeap Int (Int, Int) -> Int
        step finished queue = case H.view queue of
          Nothing -> undefined
          Just ((val, pos), queue')
            | pos `M.member` finished -> step finished queue'
            | pos == end -> val
            | otherwise -> let queue'' = foldl (flip H.insert) queue' . map (val + 1, ) $ reachableNeighbors
                           in step (M.insert pos val finished) queue''
            where reachableNeighbors = filter (\p -> p `M.member` heightMap && (heightMap ! p) <= succ posChar) neighbors
                  neighbors = let (r, c) = pos in [(r-1, c), (r+1, c), (r, c-1), (r, c+1)]
                  posChar = heightMap ! pos

solve1 = solve (\hm s _ -> (0, s):map (maxBound, ) (M.keys hm))

solve2 = solve (\hm _ _ -> map swap . M.toList . M.map (\x -> if x == 'a' then 0 else maxBound) $ hm)

solution = runSolution parser solve1 solve2