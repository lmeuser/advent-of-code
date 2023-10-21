{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-cse #-}

import System.Console.CmdArgs

import Solutions.All (years)

runDay year day = ((years !! (year - 2015)) !! (day - 1)) day
runYear year = mapM_ (runDay year) [1.. length (years !! (year - 2015))]
runAll = mapM_ runYear [2015..2022]


data Arguments = Arguments {
    year :: Maybe Int
  , day :: Maybe Int
  }
  deriving (Data, Typeable, Show)

arguments = Arguments { year = def, day = def }

validYear year = year >= 2022 && year <= 2022
validDay year day = validYear year && day >= 1 && day <= length (years !! (year - 2015))

main :: IO ()
main = do
    args <- cmdArgs arguments
    case (year args, day args) of
        (Nothing, Just _) -> putStrLn "You need to provide a year if you provide a day"
        (Just year, Nothing) -> if validYear year
                                then runYear year
                                else putStrLn "invalid year"
        (Just year, Just day) -> if validDay year day
                                 then runDay year day
                                 else putStrLn "invalid year/day"
        (Nothing, Nothing) -> runAll
