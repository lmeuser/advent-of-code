module Solutions.Year2021.Day9 where

import Data.List (sort, group, sortBy)
import Data.Map ((!), fromList, insert, Map, toList)
import qualified Data.Map as M (lookup, map, empty)
import Data.Char (digitToInt)
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared (runSolution, listsToMap)

parser = toMap <$> sepBy (many (digitToInt <$> digitChar)) newline
  where toMap raw = (listsToMap raw, length raw, length (head raw))

isLowPoint mapData (a, b) = mapData ! (a, b) < minimum neighbors
  where neighbors = map fromJust . filter (/= Nothing) . map (`M.lookup` mapData) $ [(a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1)]

findLowPoints mapData rowCount colCount = filter (isLowPoint mapData) $ [(row, col) | row <- [0..rowCount - 1], col <- [0..colCount - 1]]

solve1 (mapData, rowCount, colCount) = sum . map ((+ 1) . (mapData !)) $ lowPoints
  where lowPoints = findLowPoints mapData rowCount colCount


-- I somehow feel like this is horribly inefficient and inelegant, but I
-- couldn't think of a better way quickly. I suspect there is one that has you
-- start a search for connected points from the low points or something like
-- that.
-- basic strategy here: iterate over the map, for fields != 9 set a basin number:
-- If it borders one basin, use that number. If it doesn't border any, use a
-- new number. If it borders different basins, make a note and use one of them.
-- Use the notes later to merge the different basin numbers into one basin
findBasins mapData rows cols = helper M.empty M.empty 0 [(r, c) | r <- [0..rows - 1], c <- [0..cols - 1]]
  where helper basinMap mergeMap latest [] = merge (basinMap, mergeMap, latest)
        helper basinMap mergeMap latest (coords@(row, col):rest) = if mapData ! (row, col) == 9
                                                                   then helper basinMap mergeMap latest rest
                                                                   else helper basinMap' mergeMap' latest' rest
          where basinMap' = insert coords basin basinMap
                (basin, latest') = case neighborBasin of
                                      Just b -> (b, latest)
                                      Nothing -> (latest + 1, latest + 1)
                (neighborBasin, mergeMap') = case map fromJust . filter (/= Nothing) $ [M.lookup (row - 1, col) basinMap, M.lookup (row, col - 1) basinMap] of
                                                [] -> (Nothing, mergeMap)
                                                [a] -> (Just a, mergeMap)
                                                [a, b] -> if a == b
                                                          then (Just a, mergeMap)
                                                          else (Just (min a b), insert (max a b) (min a b) mergeMap)
        merge (basinMap, mergeMap, latest) = M.map (mergeMap' !) basinMap
          where mergeMap' = findRoots mergeMap [1..latest]
                findRoots mm [] = mm
                findRoots mm (x:xs) = findRoots (insert x (rootFor x) mm) xs
                  where rootFor x = case M.lookup x mm of
                                      Just y -> if x == y then x else rootFor y
                                      Nothing -> x

basinSizes = map length . group . sort . map snd . toList

solve2 (mapData, rows, cols) = product . take 3 . sortBy (flip compare) . basinSizes $ findBasins mapData rows cols

solution = runSolution parser solve1 solve2
