module Solutions.Year2021.Day3 where

import Data.List (transpose)
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)


parser = endBy (map (== '1') <$> many (char '0' <|> char '1')) newline

binToDec = foldl (\x y -> 2 * x + fromEnum y) 0

mostCommonBit l = sum (map (\x -> if x then 1 else -1) l) >= 0

solve1 numbers = gamma * epsilon
  where bin     = map mostCommonBit (transpose numbers)
        gamma   = binToDec bin
        epsilon = binToDec (map not bin)

filterNumbers keepCommon index numbers = filter ((== keep) . (!! index)) numbers
  where keep = (mostCommonBit . (!! index) . transpose $ numbers) == keepCommon

filterDown _          _     [x] = x
filterDown keepCommon index xs  = filterDown keepCommon (index + 1) (filterNumbers keepCommon index xs)

solve2 xs = binToDec oxy * binToDec co2
  where oxy = filterDown True 0 xs
        co2 = filterDown False 0 xs

solution = runSolution parser solve1 solve2
