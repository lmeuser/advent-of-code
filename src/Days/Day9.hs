module Days.Day9 where

import Data.List (group, sort)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Shared (runSolution)

parser = concat <$> sepBy (moves <$> upperChar <* char ' ' <*> decimal) newline
  where moves 'L' n = replicate n (-1, 0)
        moves 'R' n = replicate n (1, 0)
        moves 'U' n = replicate n (0, -1)
        moves 'D' n = replicate n (0, 1)

tailMove hx hy tx ty
  | abs xDist <= 1 && abs yDist <= 1 = (0, 0)
  | xDist == 0 = (0, clamp yDist)
  | yDist == 0 = (clamp xDist, 0)
  | otherwise = (clamp xDist, clamp yDist)
  where xDist = hx - tx
        yDist = hy - ty
        clamp = min 1 . max (-1)

moveRope rope move ps = (rope', last rope':ps)
  where rope' = step rope move
        step [(tx, ty)] (mx, my) = [(tx + mx, ty + my)]
        step rope@((hx, hy):(tx, ty):_) (mx, my) = (hx', hy'):step (tail rope) (tailMove hx' hy' tx ty)
          where (hx', hy') = (hx + mx, hy + my)

solveFor n = length . group . sort . snd . foldl (\(rope, ps) move -> moveRope rope move ps) (replicate n (0, 0), [])

solution = runSolution parser (solveFor 2) (solveFor 10)
