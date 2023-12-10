{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import Data.Maybe (fromMaybe)
import System.Console.CmdArgs
import System.TimeIt (timeItNamed)

import Solutions.All (years)

runDay time year day = let label = ("day " ++ show year ++ "-" ++ show day)
                       in wrap time label $ ((years !! (year - 2015)) !! (day - 1)) day year
runYear time year = let label = "year " ++ show year
                    in wrap time label $ mapM_ (runDay time year) [1.. length (years !! (year - 2015))]
runAll time = wrap time "total" $ mapM_ (runYear time) [2015..2023]


data Arguments = Arguments {
    year :: Maybe Int
  , day :: Maybe Int
  , benchmark :: Maybe Bool
  }
  deriving (Data, Typeable, Show)

arguments = Arguments { year = def, day = def, benchmark = def }

validYear year = year >= 2015 && year <= 2023
validDay year day = validYear year && day >= 1 && day <= length (years !! (year - 2015))

wrap benchmark label f = if benchmark then timeItNamed ("time (" ++ label ++ ")") f else f

main :: IO ()
main = do
    args <- cmdArgs arguments
    let time = fromMaybe False (benchmark args)
    case (year args, day args) of
        (Nothing, Just _) -> putStrLn "You need to provide a year if you provide a day"
        (Just year, Nothing) -> if validYear year
                                then runYear time year
                                else putStrLn "invalid year"
        (Just year, Just day) -> if validDay year day
                                 then runDay time year day
                                 else putStrLn "invalid year/day"
        (Nothing, Nothing) -> runAll time
