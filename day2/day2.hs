data RPS = Rock | Paper | Scissors
  deriving (Show, Eq)

instance Ord RPS where
  Rock <= Scissors = False
  Paper <= Rock = False
  Scissors <= Paper = False
  _ <= _ = True

toRPS x | x `elem` "AX" = Rock
        | x `elem` "BY" = Paper
        | x `elem` "CZ" = Scissors


pointsFromChoice Rock = 1
pointsFromChoice Paper = 2
pointsFromChoice Scissors = 3

pointsFromResult a b = case compare a b of
  LT -> 6
  EQ -> 3
  GT -> 0

pointsFromGame (a, b) = pointsFromChoice b + pointsFromResult a b


solution1 = sum . map pointsFromGame

-- too lazy to change the parser, so R = lose, P = draw, S = win
result Rock Rock = Scissors
result Rock Scissors = Paper
result Paper x = x
result Scissors Rock = Paper
result Scissors Scissors = Rock
result x Paper = x

solution2 = solution1 . map (\(a, b) -> (a, result a b))



firstTwo (a:b:_) = (a, b)
inpToRPS = map (firstTwo . map (toRPS . head) . words) . lines

main = do
  input <- readFile "input.txt"
  let rpsData = inpToRPS input
  
  putStrLn . show . solution1 $ rpsData
  putStrLn . show . solution2 $ rpsData
