module Solutions.Year2023.Day16 where

import Data.Array
import Prelude hiding (Left, Right)
import Text.Megaparsec
import Text.Megaparsec.Char

import Shared

data Tile = Empty | MirrorForward | MirrorBackward | SplitHoriz | SplitVert

data Direction = Up | Down | Left | Right
  deriving Eq

parser = buildArray <$> sepBy line newline
  where line = some ((Empty <$ char '.') <|> (MirrorForward <$ char '/') <|> (MirrorBackward <$ char '\\') <|> (SplitHoriz <$ char '-') <|> (SplitVert <$ char '|'))
        buildArray xs = listArray ((0, 0), (length xs - 1, length (head xs) - 1)) $ concat xs

next'' (r, c) Left = (r, c - 1)
next'' (r, c) Right = (r, c + 1)
next'' (r, c) Up = (r - 1, c)
next'' (r, c) Down = (r + 1, c)

next' (r, c) d = (case d of
                    Left -> (r, c - 1)
                    Right -> (r, c + 1)
                    Up -> (r - 1, c)
                    Down -> (r + 1, c)
                  , d)

next pos dir Empty = [next' pos dir]
next pos dir SplitHoriz
  | dir `elem` [Left, Right] = [next' pos dir]
  | otherwise = [next' pos Left, next' pos Right]
next pos dir SplitVert
  | dir `elem` [Up, Down] = [next' pos dir]
  | otherwise = [next' pos Up, next' pos Down]
next pos Up MirrorForward = [next' pos Right]
next pos Down MirrorForward = [next' pos Left]
next pos Left MirrorForward = [next' pos Down]
next pos Right MirrorForward = [next' pos Up]
next pos Up MirrorBackward = [next' pos Left]
next pos Down MirrorBackward = [next' pos Right]
next pos Left MirrorBackward = [next' pos Up]
next pos Right MirrorBackward = [next' pos Down]

calc inp s@(sp, sd) = length . filter (not . null) . elems . step [s] $ listArray (bounds inp) (repeat []) // [(sp, [sd])]
  where step [] a = a
        step (x:xs) a = let (p, dir) = x
                            foo = next p dir (inp ! p)
                            bar = filter (\(p, d) -> inRange (bounds a) p && d `notElem` (a ! p)) foo
                        in step (bar ++ xs) (accum (flip (:)) a bar)

solve1 inp = calc inp ((0, 0), Right)

solve2 inp = let (r, c) = snd . bounds $ inp
                 top = map (\n -> ((0, n), Down)) [0..c]
                 bottom = map (\n -> ((r, n), Up)) [0..c]
                 left = map (\n -> ((n, 0), Right)) [0..r]
                 right = map (\n -> ((n, c), Left)) [0..r]
             in maximum . map (calc inp) $ top ++ bottom ++ left ++ right

solution = runSolution parser solve1 solve2
