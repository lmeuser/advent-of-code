module Solutions.Year2019.Day5 where

import Solutions.Year2019.Intcode
import Shared

solve n = last . snd . runIntcode [n]

solution = runSolution intcodeParser (solve 1) (solve 5)
