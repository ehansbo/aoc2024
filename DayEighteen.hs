import DayZero
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Control.Monad.State as State
import Data.Maybe

maxXY = 70

type QState = State.State (S.Set Coord, Seq.Seq (Coord, Int))

main :: IO ()
main = do
    input <- manuallyParse "d18.txt" "\n" parse
    let first = S.fromList $ take 1024 input
    let second = drop 1024 input
    print $ solve1 first
    print $ solve2 first second

solve2 :: S.Set Coord -> [Coord] -> Coord
solve2 coords (x:xs) =
    let coords' = S.insert x coords
        maybeResults = State.evalState (run coords') (S.empty, Seq.singleton ((0, 0), 0))
    in if maybeResults == Nothing then x else solve2 coords' xs

solve1 :: S.Set Coord -> Maybe Int
solve1 coords = State.evalState (run coords) (S.empty, Seq.singleton ((0, 0), 0))

run :: S.Set Coord -> QState (Maybe Int)
run walls = do
    visited <- getVisited
    maybeNext <- pop
    if maybeNext == Nothing then return Nothing else
        let Just (coord, len) = maybeNext
        in 
            if coord == (maxXY, maxXY) then return (Just len)
            else if coord `S.member` visited then run walls
            else do
                ns <- validate (neighbors coord) walls
                let ns' = zip ns (repeat $ len + 1)
                push ns'
                run walls

getVisited :: QState (S.Set Coord)
getVisited = do
    (visited, _) <- State.get
    return visited

push :: [(Coord, Int)] -> QState ()
push cs = do
    (visited, queue) <- State.get
    let queue' = queue Seq.>< (Seq.fromList cs)
    State.put (visited, queue')

validate :: [Coord] -> S.Set Coord -> QState [Coord]
validate cs walls = do
    (visited, _) <- State.get
    return $ filter (\c -> (not . outOfBounds) c && not (c `S.member` visited) && not (c `S.member` walls)) cs

outOfBounds :: Coord -> Bool
outOfBounds (x, y) = x < 0 || x > maxXY || y < 0 || y > maxXY

pop :: QState (Maybe (Coord, Int))
pop = do
    (visited, queue) <- State.get
    let maybePopped = Seq.lookup 0 queue
    if maybePopped == Nothing then
        return Nothing
    else do
        let (Just (coord, len)) = maybePopped
        State.put (S.insert coord visited, Seq.drop 1 queue)
        return $ Just (coord, len)


parse :: String -> Coord
parse str = read $ "(" ++ str ++ ")"