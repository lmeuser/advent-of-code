module Shared where

runSolution :: (Show b1, Show b2) => (String -> b1) -> (String -> b2) -> Int -> IO ()
runSolution solve1 solve2 day = do
    let dayStr = show day
    input <- readFile ("inputs/day" ++ dayStr ++ ".txt")
    putStrLn ("\n=== Day " ++ dayStr ++ " ===")
    print . solve1 $ input
    print . solve2 $ input
    putStrLn ""
