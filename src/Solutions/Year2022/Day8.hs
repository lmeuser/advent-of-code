module Solutions.Year2022.Day8 where

import Data.Char (digitToInt)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution, listsToMap)

parser = listsToMap <$> sepBy (many (digitToInt <$> digitChar)) newline

-- ugliest solution on earth

left (r, c) = (r, c - 1)
right (r, c) = (r, c + 1)
up (r, c) = (r - 1, c)
down (r, c) = (r + 1, c)

solve1 m = M.foldlWithKey (\c pos val -> c + if val >= fullMap ! pos then 1 else 0) 0 m
  where (maxRow, maxCol) = fst (M.findMax m)
        fullMap = M.unionsWith min [leftMap, rightMap, upMap, downMap]
        leftMap = foldl (\r' row -> mapper r' 0 left (row, maxCol)) M.empty [0..maxRow]
        rightMap = foldl (\r' row -> mapper r' 0 right (row, 0)) M.empty [0..maxRow]
        upMap = foldl (\r' col -> mapper r' 0 up (maxRow, col)) M.empty [0..maxRow]
        downMap = foldl (\r' col -> mapper r' 0 down (0, col)) M.empty [0..maxCol]
        mapper :: M.Map (Int, Int) Int -> Int -> ((Int, Int) -> (Int, Int)) -> (Int, Int) -> M.Map (Int, Int) Int
        mapper result maxVal step pos = case M.lookup pos m of
          Nothing -> result
          Just value -> mapper (M.insert pos maxVal result) maxVal' step (step pos)
            where maxVal' = max (value + 1) maxVal


-- lol brute force

solve2 m = maximum . map scenicScore $ [(r, c) | r <- [0..maxRow], c <- [0..maxCol]]
  where (maxRow, maxCol) = fst (M.findMax m)
        scenicScore pos = product $ map (dist pos) [left, right, up, down]
        dist pos step = distStep (m ! pos) 0 step (step pos)
        distStep val acc step pos = case M.lookup pos m of
          Nothing -> acc
          Just x -> if x >= val then acc + 1 else distStep val (acc + 1) step (step pos)


-- test m = mapAll m M.empty 0 (\(r, c) -> (r, c + 1)) (0, 0)


solution = runSolution parser solve1 solve2
