module Solutions.Year2023.Day6 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer

import Shared

parser = (,) <$> times <*> distances
    where times = line "Time:"
          distances = line "Distance:"
          line start = string start *> space1 *> sepEndBy decimal space1

solveRace (time, target) = floor ((time / 2) + root) - ceiling ((time / 2) - root) + 1
    where root = sqrt ((time ^ 2)/4 - target)

solve1 = product . map solveRace . uncurry zip

solve2 = solveRace . fixParse
    where fixParse (times, dists) = (fix times, fix dists)
          fix = read . concatMap (show . truncate)

solution = runSolution parser solve1 solve2
