import System.Environment (getArgs)
import Text.Read (readMaybe)

import qualified Solutions.Year2022.Day1
import qualified Solutions.Year2022.Day2
import qualified Solutions.Year2022.Day3
import qualified Solutions.Year2022.Day4
import qualified Solutions.Year2022.Day5
import qualified Solutions.Year2022.Day6
import qualified Solutions.Year2022.Day7
import qualified Solutions.Year2022.Day8
import qualified Solutions.Year2022.Day9
import qualified Solutions.Year2022.Day10
import qualified Solutions.Year2022.Day11
import qualified Solutions.Year2022.Day12
import qualified Solutions.Year2022.Day13
import qualified Solutions.Year2022.Day14
import qualified Solutions.Year2022.Day15
-- import qualified Solutions.Year2022.Day16

days = [ Solutions.Year2022.Day1.solution
       , Solutions.Year2022.Day2.solution
       , Solutions.Year2022.Day3.solution
       , Solutions.Year2022.Day4.solution
       , Solutions.Year2022.Day5.solution
       , Solutions.Year2022.Day6.solution
       , Solutions.Year2022.Day7.solution
       , Solutions.Year2022.Day8.solution
       , Solutions.Year2022.Day9.solution
       , Solutions.Year2022.Day10.solution
       , Solutions.Year2022.Day11.solution
       , Solutions.Year2022.Day12.solution
       , Solutions.Year2022.Day13.solution
       , Solutions.Year2022.Day14.solution
       , Solutions.Year2022.Day15.solution
    --    , Solutions.Day16.solution
       ]

runDay day = (days !! (day - 1)) day

main :: IO ()
main = do
    args <- getArgs
    let existingDays = [1..length days]
    if null args then
         mapM_ runDay existingDays
    else
        case readMaybe . head $ args of
            Nothing -> putStrLn "You need to provide either no arguments, or a day number"
            Just x -> if x `elem` existingDays then
                          runDay x
                      else
                          putStrLn ("Day " ++ show x ++ " does not exist")
