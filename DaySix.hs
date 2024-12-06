import DayZero
import Data.List
import qualified Data.Set as S

data Direction = North | South | West | East
    deriving (Eq, Ord)
type Coord = (Int, Int)

nextPos :: Direction -> Coord -> Coord
nextPos East (x, y) = (x+1, y)
nextPos West (x, y) = (x-1, y)
nextPos North (x, y) = (x, y-1)
nextPos South (x, y) = (x, y+1)

rotate :: Direction -> Direction
rotate East = South
rotate South = West
rotate West = North
rotate North = East

main :: IO ()
main = do
    rows <- splitFile "d6.txt" "\n"
    print $ solve1 rows
    print $ solve2 rows

solve1 :: [String] -> Int
solve1 rows = 
    let obstacles = getObstacles rows
        start = getStart rows
        isOutside (x, y) dir = x < 0 || y < 0 || x == (length $ head rows) || y == (length rows)
    in length $ nub $ getVisited start North obstacles isOutside

solve2 :: [String] -> Int
solve2 rows =
    let obstacles = getObstacles rows
        start = getStart rows
        isOutside (x, y) dir = 
            let xOut = length $ head rows
                yOut = length rows
            in  x < 0 || y < 0 || x == xOut || y == yOut
        possibleObstacleAdditions = filter (\c -> c `S.notMember` obstacles && c /= start) [(x, y) | x<-[0..length (head rows)-1], y <- [0..length rows-1]]
    in length $ filter (\o -> stuckInLoop S.empty start North (S.insert o obstacles) isOutside) possibleObstacleAdditions

stuckInLoop :: S.Set (Coord, Direction) -> Coord -> Direction -> S.Set Coord -> (Coord -> Direction -> Bool) -> Bool
stuckInLoop visited current dir obstacles isOutside 
    | (current, dir) `S.member` visited = True
    | nextPos dir current `S.member` obstacles = stuckInLoop visited current (rotate dir) obstacles isOutside
    | isOutside current dir = False
    | otherwise = stuckInLoop ((current, dir) `S.insert` visited) (nextPos dir current) dir obstacles isOutside

getVisited :: Coord -> Direction -> S.Set Coord -> (Coord -> Direction -> Bool) -> [Coord]
getVisited current dir obstacles isOutside 
    | nextPos dir current `S.member` obstacles = getVisited current (rotate dir) obstacles isOutside
    | isOutside current dir = []
    | otherwise = current : (getVisited (nextPos dir current) dir obstacles isOutside)


getObstacles :: [String] -> S.Set Coord
getObstacles rows =
    let allCoordinates = getCoords rows
    in S.fromList $ map fst (filter (\(_, ch) -> ch == '#') allCoordinates)

getStart :: [String] -> Coord
getStart rows = 
    let allCoordinates = getCoords rows
        ((x, y), _):[] = filter (\(_, ch) -> ch == '^') allCoordinates
    in (x, y)

getCoords :: [String] -> [(Coord, Char)]
getCoords rows =
    let zipped = zip [0..] (map (zip [0..]) rows)
    in concat $ map (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) zipped