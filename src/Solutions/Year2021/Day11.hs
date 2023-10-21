{-# LANGUAGE TupleSections #-}

module Solutions.Year2021.Day11 where

import Data.Char (digitToInt)
import Data.List (elemIndex)
import Data.Map ((!), fromList, insert, keys, member, size)
import qualified Data.Map as M (filter, map)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared (runSolution, listsToMap)


parser = listsToMap <$> sepBy (many (digitToInt <$> digitChar)) newline

neighbors (x, y) = map (x-1,) [y-1..y+1] ++ [(x, y-1), (x, y+1)] ++ map (x+1,) [y-1..y+1]

step dataMap = helper dataMap (keys dataMap)
  where helper m [] = M.map (\x -> if x > 9 then 0 else x) m
        helper m (x:xs) = helper m' xs'
          where val = m ! x + 1
                m' = insert x val m
                xs' = if val == 10 then existingNeighbors x ++ xs else xs
                existingNeighbors = filter (`member` dataMap) . neighbors

solve1 = sum . map countFlashes . take 101 . iterate step
  where countFlashes = size . M.filter (== 0)

solve2 = fromJust . elemIndex True . map (all (== 0)) . iterate step

solution = runSolution parser solve1 solve2
