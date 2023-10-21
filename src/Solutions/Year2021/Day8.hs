module Solutions.Year2021.Day8 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)
import Data.List ((\\), intersect, sort, elemIndex)
import Data.Maybe (fromJust)

--   let lines8 = lines input8

parser = sepBy ((,) <$> endBy (many lowerChar) (char ' ') <* string "| " <*> sepBy (many lowerChar) (char ' ')) newline

solve1 = sum . map (length . filter (\x -> length x `elem` [2, 3, 4, 7]) . snd)

solveLine inputDigits = let fiveLong = filterByLength 5 inputDigits
                            sixLong = filterByLength 6 inputDigits
                            one = identifyByLength 2 inputDigits
                            four = identifyByLength 4 inputDigits
                            seven = identifyByLength 3 inputDigits
                            eight = identifyByLength 7 inputDigits
                            a = head (seven \\ one)
                            six = head . filter ((== 1) . length . intersect one) $ sixLong
                            c = head (eight \\ six)
                            f = head (one \\ [c])
                            two = head . filter (not . (f `elem`)) $ fiveLong
                            five = head . filter (not . (c `elem`)) $ fiveLong
                            three = head (fiveLong \\ [two, five])
                            e = head ((eight \\ three) \\ five)
                            nine = head . filter (not . (e `elem`)) $ sixLong
                            zero = head (sixLong \\ [six, nine])
                        in map sort [zero, one, two, three, four, five, six, seven, eight, nine]
  where filterByLength l = filter ((== l) . length)
        identifyByLength l = head . filterByLength l


solve2 = sum . map lineValue
  where lineValue (input, output) = let mapping = solveLine input
                                        digits = map (fromJust . (`elemIndex` mapping) . sort) output
                                    in foldl (\a b -> a * 10 + b) 0 digits

solution = runSolution parser solve1 solve2
