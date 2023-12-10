module Solutions.Year2019.Day7 where

import Data.List (permutations)

import Solutions.Year2019.Intcode
import Shared

solve i = maximum . map calculate . permutations $ [0, 1, 2, 3, 4]
  where calculate = foldl step 0
        step signal phase = head . snd . runIntcode [phase, signal] $ i

solution = runSolution intcodeParser (const ()) solve
