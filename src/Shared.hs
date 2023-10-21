module Shared where

import Data.Map (fromList, Map)
import Data.Void ( Void )
import Text.Megaparsec ( Parsec, runParser )


type Parser = Parsec Void String

newtype DisplayString = DisplayString String

instance Show DisplayString where
    show (DisplayString s) = s

runSolution :: (Show b1, Show b2) => Parser a -> (a -> b1) -> (a -> b2) -> Int -> Int -> IO ()
runSolution parser solve1 solve2 day year = do
    let dayStr = show day
    let yearStr = show year
    input <- readFile ("inputs/day" ++ dayStr ++ ".txt")
    putStrLn ("\n=== Year " ++ yearStr ++ " -- Day " ++ dayStr ++ " ===")
    case runParser parser "" input of
        Left e -> putStrLn ("parser error: " ++ show e)
        Right parsed -> do print . solve1 $ parsed
                           print . solve2 $ parsed
    putStrLn ""

buildMapList coords = [((row, col), value) | (row, rowDigits) <- zip [0..] coords, (col, value) <- zip [0..] rowDigits]

listsToMap :: Integral a => [[b]] -> Map (a, a) b
listsToMap = fromList . buildMapList
