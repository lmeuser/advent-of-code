module Solutions.Year2023.Day6 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = (,) <$> times <*> distances
    where times = line "Time:"
          distances = line "Distance:"
          line start = string start *> space1 *> sepEndBy decimal space1

solveRace (time, target) = length . filter (> target) . distances $ time
    where distances time = map (distance time) [1..time-1]
          distance time press = (time - press) * press

solve1 = product . map solveRace . uncurry zip

solve2 = solveRace . fixParse
    where fixParse (times, dists) = (fix times, fix dists)
          fix = read . concatMap show

solution = runSolution parser solve1 solve2
