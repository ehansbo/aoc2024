import DayZero
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Control.Monad.State

data Object = Robot | Wall | Box | WideBox Direction
    deriving (Eq, Show)

main = do
    (mapStr:pathStr:[]) <- splitFile "d15.txt" "\n\n"
    let pathStr' = filter (/= '\n') pathStr
    let (inputMap, xMax, yMax) = getCoords (splitOn "\n" mapStr)
    let inputMap' = filter (\(_, c) -> c /= '.') $ inputMap
    let paths = map parseDir pathStr'
    print $ solve1 inputMap' paths

    let mapStr2 = widen mapStr
    let (inputMap2, xMax2, yMax2) = getCoords (splitOn "\n" mapStr2)
    let inputMap2' = filter (\(_, c) -> c /= '.') $ inputMap2
    print $ solve1 inputMap2' paths


widen :: String -> String
widen = concat . map widenChar

widenChar :: Char -> String
widenChar '#' = "##"
widenChar 'O' = "[]"
widenChar '.' = ".."
widenChar '@' = "@."
widenChar '\n' = "\n"

printMap :: M.Map Coord Object -> Int -> Int -> String
printMap coordMap xMax yMax = concat $ map (\(x, y) -> (if x == 0 then "\n" else []) ++ printMaybeObj (coordMap M.!? (x, y))) [(x, y) | y <- [0..yMax], x <- [0..xMax]]

solveTmp asList dirs =
    let robotCoord = (fst . head . filter (\(_, c) -> c == '@')) asList
        coordMap = M.fromList $ map (\(coord, c) -> (coord, toObject c)) asList
    in execState (run robotCoord dirs) coordMap

--solve1 :: [(Coord, Char)] -> [Direction] -> Int
solve1 asList dirs =
    let robotCoord = (fst . head . filter (\(_, c) -> c == '@')) asList
        coordMap = M.fromList $ map (\(coord, c) -> (coord, toObject c)) asList
    in score $ execState (run robotCoord dirs) coordMap

score :: M.Map Coord Object -> Int
score coordMap = sum $ map (\((x, y), _) -> x + y*100) (filter (\(coord, object) -> object == Box || object == (WideBox East)) (M.toList coordMap))

printMaybeObj :: Maybe Object -> String
printMaybeObj Nothing = "."
printMaybeObj (Just Robot) = "@"
printMaybeObj (Just Wall) = "#"
printMaybeObj (Just Box) = "O"
printMaybeObj (Just (WideBox East)) = "["
printMaybeObj (Just (WideBox West)) = "]"

run :: Coord -> [Direction] -> State (M.Map Coord Object) ()
run robotCoord (dir:dirs) = do
    possible <- tryMove robotCoord dir
    if possible then run (nextPos dir robotCoord) dirs else run robotCoord dirs
run _ [] = return ()

tryMove :: Coord -> Direction -> State (M.Map Coord Object) Bool
tryMove coord dir = do
    coordMap <- get
    let maybeObject = coordMap M.!? coord
    case (maybeObject, dir == North || dir == South) of 
        (Nothing, _) -> return True
        (Just Wall, _) -> return False
        (Just (WideBox otherDir),  True) -> do
            let otherCoord = nextPos otherDir coord
            possible1 <- tryMove (nextPos dir coord) dir
            possible2 <- tryMove (nextPos dir otherCoord) dir
            if possible1 && possible2 then do
                coordMapUpdated <- get
                let coordMapUpdated' = ((M.insert (nextPos dir coord) (WideBox otherDir)) . (M.delete coord)) coordMapUpdated
                put $ ((M.insert (nextPos dir otherCoord) (WideBox (if otherDir == West then East else West))) . (M.delete otherCoord)) coordMapUpdated'
                return True
            else do
                put coordMap -- make sure if one was possible we should revert the map
                return False
        (Just o, _) -> do
            possible <- tryMove (nextPos dir coord) dir
            if possible then do
                coordMapUpdated <- get
                put $ ((M.insert (nextPos dir coord) o) . (M.delete coord)) coordMapUpdated
                return True
            else return False

toObject :: Char -> Object
toObject '@' = Robot
toObject '#' = Wall
toObject 'O' = Box
toObject '[' = WideBox East
toObject ']' = WideBox West


parseDir :: Char -> Direction
parseDir '<' = West
parseDir '^' = North
parseDir '>' = East
parseDir 'v' = South
