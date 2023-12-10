{-# LANGUAGE TupleSections #-}
module Solutions.Year2023.Day10 where

import Data.Array
import Data.List (find)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

parser = buildArray <$> sepBy (some mapItem) newline
  where mapItem = satisfy (`elem` "|-LJ7F.S")
        buildArray rows@(cols:_) = listArray ((0, 0), (length rows - 1, length cols - 1)) (concat rows)

findPipe a = start:step start first
  where step p c = let n = next p c
                   in c:if n == start then [] else step c n
        start@(sr, sc) = fst . fromJust . find ((== 'S') . snd) . assocs $ a
        first = fst . head . filter isConnected $ neighbors
          where isConnected (pos, val) = inRange (bounds a) pos && (a ! pos) `elem` val
                neighbors = [((sr - 1, sc), "|7FS"),
                             ((sr + 1, sc), "|JLS"),
                             ((sr, sc - 1), "-LFS"),
                             ((sr, sc + 1), "-J7S")]
        next (pr, pc) (r, c) = case (a ! (r, c), (r - pr, c - pc)) of
                          ('|', (1, 0)) -> (r + 1, c)
                          ('|', (-1, 0)) -> (r - 1, c)
                          ('-', (0, 1)) -> (r, c + 1)
                          ('-', (0, -1)) -> (r, c - 1)
                          ('L', (1, 0)) -> (r, c + 1)
                          ('L', (0, -1)) -> (r - 1, c)
                          ('J', (1, 0)) -> (r, c - 1)
                          ('J', (0, 1)) -> (r - 1, c)
                          ('7', (-1, 0)) -> (r, c - 1)
                          ('7', (0, 1)) -> (r + 1, c)
                          ('F', (-1, 0)) -> (r, c + 1)
                          ('F', (0, -1)) -> (r + 1, c)

solve1 = (`div` 2) . length . findPipe


-- this is a pretty ugly and slow solution I think, but I'm too lazy to do it better right now
-- basic idea: expand each field in the original map to a 3x3 with the original pipe piece
-- in the center and connecting pipes on the sides. Then we can just check for fields connected to (0, 0)
-- to get the outside ones. (Without expanding, I think we'd need to check pipe directions etc)
solve2 a = rangeSize (bounds a) - (S.size . collapse . step (Seq.singleton (0, 0)) $ M.empty)
  where step q m = case Seq.viewl q of
                     (r, c) Seq.:< rest -> let shouldVisit pos = filled && inRange (bounds a3) pos && pos `M.notMember` m
                                               neighbors = filter shouldVisit [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]
                                               filled = a3 ! (r, c) == ' '
                                               m' = M.insert (r, c) filled m
                                               q' = rest Seq.>< Seq.fromList neighbors
                                            in if (r, c) `M.member` m
                                               then step rest m
                                               else step q' m'
                     Seq.EmptyL -> m
        a3 = array ((0, 0), (or * 3 + 2, oc * 3 + 2)) . concatMap expand . range . bounds $ a
        (or, oc) = snd (bounds a)
        expand (r, c)
          | (r, c) `S.member` pipe = map (, ' ') [(r3, c3), (r3 + 2, c3), (r3, c3 + 2), (r3 + 2, c3 + 2)] ++
                     [((r3, c3 + 1), if orig `elem` "|JLS" then '|' else ' '),
                      ((r3 + 1, c3), if orig `elem` "-J7S" then '-' else ' '),
                      ((r3 + 2, c3 + 1), if orig `elem` "|7FS" then '|' else ' '),
                      ((r3 + 1, c3 + 2), if orig `elem` "-FLS" then '-' else ' '),
                      ((r3 + 1, c3 + 1), orig)]
          | otherwise = map (, ' ') (range ((r3, c3), (r3 + 2, c3 + 2)))
          where (r3, c3) = (r * 3, c * 3)
                orig = a ! (r, c)
                pipe = S.fromList . findPipe $ a
        collapse = M.foldrWithKey' step S.empty 
          where step (r, c) v s = if v then S.insert (r `div` 3, c `div` 3) s else s

solution = runSolution parser solve1 solve2
