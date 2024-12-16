import DayZero
import qualified Data.PQueue.Min as Q
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Current = Current {score :: Int, pos :: Coord, dir :: Direction, path :: S.Set Coord}
    deriving (Eq)

instance Ord Current where
    compare c1 c2 = compare (score c1) (score c2)


main :: IO ()
main = do
    (coordList, _, _) <- readAsMap "d16.txt"
    let startCoord = getStartCoord coordList
    let endCoord = getEndCoord coordList
    let inputMap = S.fromList $ ((map fst) . (filter (\(_, c) -> c == '#'))) coordList
    print $ solve1 startCoord endCoord inputMap
    print $ solve2 startCoord endCoord inputMap

solve2 :: Coord -> Coord -> S.Set Coord -> Int
solve2 start end walls =
    let current = Current 0 start East S.empty
        visited = run2 end walls M.empty Nothing S.empty (Q.singleton current)
    in  S.size visited + 1 -- Last is not included

solve1 :: Coord -> Coord -> S.Set Coord -> Int
solve1 start end walls =
    let current = Current 0 start East S.empty
    in run end walls S.empty (Q.singleton current)

run :: Coord -> S.Set Coord -> S.Set (Coord, Direction) -> Q.MinQueue Current -> Int
run end walls visited queue = 
    let (current, queue') = Q.deleteFindMin queue
        nextCoord = nextPos (dir current) (pos current)
        nexts = [Current (score current + 1000) (pos current) (rotateR $ dir current) (path current), Current (score current + 1000) (pos current) (rotateL $ dir current) (path current)]
        nexts' = if nextCoord `S.member` walls then nexts else (Current (score current + 1) (nextCoord) (dir current) (S.insert (pos current) (path current))) : nexts
        queue'' = Q.union queue' (Q.fromAscList nexts')
    in if (pos current, dir current) `S.member` visited then run end walls visited queue' else if (pos current) == end then (score current) else run end walls (S.insert (pos current, dir current) visited) queue''

run2 :: Coord -> S.Set Coord -> M.Map (Coord, Direction) Int -> Maybe Int -> S.Set Coord -> Q.MinQueue Current -> S.Set Coord
run2 end walls visited maybeMax results queue = 
    let (current, queue') = Q.deleteFindMin queue
        nextCoord = nextPos (dir current) (pos current)
        nexts = [Current (score current + 1000) (pos current) (rotateR $ dir current) (path current), Current (score current + 1000) (pos current) (rotateL $ dir current) (path current)]
        nexts' = if nextCoord `S.member` walls then nexts else (Current (score current + 1) (nextCoord) (dir current) (S.insert (pos current) (path current))) : nexts
        queue'' = Q.union queue' (Q.fromAscList nexts')
    in 
        if maybeMax /= Nothing && fromJust maybeMax < (score current)
            then results
        else if M.findWithDefault 999999 (pos current, dir current) visited < (score current) 
            then run2 end walls visited maybeMax results queue' 
        else if (pos current) == end 
            then run2 end walls (M.insert (pos current, dir current) (score current) visited) (Just $ score current) (S.union results (path current)) queue''
        else run2 end walls (M.insert (pos current, dir current) (score current) visited) maybeMax results queue''

rotateR :: Direction -> Direction
rotateR East = South
rotateR South = West
rotateR West = North
rotateR North = East

rotateL :: Direction -> Direction
rotateL East = North
rotateL North = West
rotateL West = South
rotateL South = East

getStartCoord :: [(Coord, Char)] -> Coord
getStartCoord list = findCoord 'S' list

getEndCoord :: [(Coord, Char)] -> Coord
getEndCoord list = findCoord 'E' list

findCoord :: Char -> [(Coord, Char)] -> Coord
findCoord char list = fst $ head $ filter (\(c, char') -> char' == char) list