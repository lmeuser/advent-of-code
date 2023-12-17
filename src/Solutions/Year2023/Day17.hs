module Solutions.Year2023.Day17 where

import Data.Array
import Data.Char (digitToInt)
import qualified Data.Heap as H
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Prelude hiding (Left, Right)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

data Direction = Left | Right | Up | Down
  deriving (Eq, Ord, Show)

parser = buildArray <$> sepBy (some (digitToInt <$> digitChar)) newline
  where buildArray rs@(c1:_) = listArray ((0, 0), (length rs - 1, length c1 - 1)) (concat rs)

neighbors (min, max) (r, c) dir dist = let dirs' = case dir of
                                                     Left -> [Left, Up, Down]
                                                     Right -> [Right, Up, Down]
                                                     Up -> [Up, Left, Right]
                                                     Down -> [Down, Left, Right]
                                           dirs''
                                             | dist == max = tail dirs'
                                             | dist < min = [dir]
                                             | otherwise = dirs'
                                           next Left = (r, c - 1)
                                           next Right = (r, c + 1)
                                           next Up = (r - 1, c)
                                           next Down = (r + 1, c)
                                       in map (\d -> (d, next d)) dirs''

-- for some reason, `helper` below not having a type annotation breaks compilation, so these are defined to make that annotation less unwieldy
type Heap = H.MinHeap (Int, Int, (Int, Int), Direction)
type Map = M.Map ((Int, Int), Direction) [(Int, Int)]

solve min max a = findFinal $ step (H.fromList [(0, 0, (0, 0), Right), (0, 0, (0, 0), Down)]) M.empty
  where findFinal m = minimum . concatMap (map snd . filter ((>= min) . fst) . \d -> m M.! (snd (bounds a), d)) $ [Right, Down]
        step h m = case H.view h of
                     Nothing -> m
                     Just ((value, dist, pos, dir), h') -> let ns = filter (inRange (bounds a) . snd) $ neighbors (min, max) pos dir dist
                                                           in uncurry step (foldl helper (h', m) ns)
                      where helper :: (Heap, Map) -> (Direction, (Int, Int)) -> (Heap, Map)
                            helper (h'', m') (dir', p') = let value' = value + a ! p'
                                                              dist' = if dir' == dir then dist + 1 else 1
                                                          in if not . any (\(dist'', v'') -> dist'' == dist' && v'' <= value') $ fromMaybe [] (m' M.!? (p', dir'))
                                                             then (H.insert (value', dist', p', dir') h'', M.insertWith (++) (p', dir') [(dist', value')] m')
                                                             else (h'', m')

solution = runSolution parser (solve 0 3) (solve 4 10)
