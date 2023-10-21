module Solutions.Year2021.Day12 where

import Data.Char (isLower, isUpper)
import qualified Data.Map as M
import Data.Map ((!))
import qualified Data.Set as S
import Data.Set ((\\))
import Data.Tuple (swap)
import Text.Megaparsec
import Text.Megaparsec.Char
import Shared (runSolution)


data Room = Start | End | Big String | Small String
  deriving (Show, Eq, Ord)

room "start" = Start
room "end" = End
room name | all isUpper name = Big name
room name = Small name

isSmall (Small _) = True
isSmall _ = False

parser = toMap <$> sepBy way newline
  where way = (,) <$> (room <$> many letterChar) <* char '-' <*> (room <$> many letterChar)
        toMap list = M.map S.fromList . M.fromListWith (++) $ (prepare list ++ prepare (map swap list))
          where prepare [] = []
                prepare ((a, b):xs) = (a, [b]):prepare xs

findRoutes mapData = helper (S.singleton Start) Start
  where helper :: S.Set Room -> Room -> [[Room]]
        helper _ End = [[End]]
        helper visited current = let visited' = if isSmall current then S.insert current visited else visited
                                 in map (current:) (concatMap (helper visited') (S.toList (mapData ! current \\ visited')))

solve1 = length . findRoutes

-- super hacky solution for part 2: just clone whatever room we wanna visit twice
solve2 dataMap = (solve1 dataMap + ) . length . concatMap routesWithClone . filter isSmall . M.keys $ dataMap
  where buildClone r = let rooms = dataMap ! r
                       in foldl (\m r -> M.insertWith S.union r (S.singleton clone) m) (M.insert clone rooms dataMap) rooms
        -- visiting clone then orig or orig then clone are two different routes
        -- here, but should be counted as one since it's the same room
        routesWithClone r = let dup = routesWithCloneDup r
                             in take (length dup `div` 2) dup
        routesWithCloneDup r = filter (\x -> clone `elem` x && r `elem` x) . findRoutes . buildClone $ r
        clone = Small "-"


solution = runSolution parser solve1 solve2
