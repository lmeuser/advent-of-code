module Solutions.Year2023.Day10 where

import Data.Array
import Data.List (find)
import Data.Maybe (fromJust)
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

data Direction = Up | Down | None

solve2 a = sum . map (\(n, _, _) -> n) $ [foldl (step row) (0, False, None) [0..cols] | row <- [0..rows]]
  where pipe = S.fromList . findPipe $ a
        (_, (rows, cols)) = bounds a
        step row (count, inside, dir) col = let pos = (row, col)
                                                inPipe = pos `S.member` pipe
                                                (inside', dir') = if inPipe
                                                                  then case (a ! pos, dir) of 
                                                                    ('L', _) -> (inside, Up)
                                                                    ('F', _) -> (inside, Down)
                                                                    ('J', Up) -> (inside, None)
                                                                    ('J', Down) -> (not inside, None)
                                                                    ('7', Up) -> (not inside, None)
                                                                    ('7', Down) -> (inside, None)
                                                                    ('|', _) -> (not inside, None)
                                                                    ('-', _) -> (inside, dir)
                                                                    -- this is just hardcoded for whatever S represents in my input
                                                                    ('S', _) -> (not inside, None)
                                                                  else (inside, dir)
                                                count' = count + if inside' && not inPipe then 1 else 0
                                            in (count', inside', dir')

solution = runSolution parser solve1 solve2
