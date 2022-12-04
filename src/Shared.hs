module Shared where

import Data.Void ( Void )
import Text.Megaparsec ( Parsec, runParser )


type Parser = Parsec Void String

runSolution :: (Show b1, Show b2) => Parser a -> (a -> b1) -> (a -> b2) -> Int -> IO ()
runSolution parser solve1 solve2 day = do
    let dayStr = show day
    input <- readFile ("inputs/day" ++ dayStr ++ ".txt")
    putStrLn ("\n=== Day " ++ dayStr ++ " ===")
    case runParser parser "" input of
        Left e -> putStrLn ("parser error: " ++ show e)
        Right parsed -> do print . solve1 $ parsed
                           print . solve2 $ parsed
    putStrLn ""
