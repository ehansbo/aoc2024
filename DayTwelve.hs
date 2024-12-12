import DayZero
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State

main :: IO ()
main = do
    (coordList, xMax, yMax) <- readAsMap "d12.txt"
    let coordMap = Map.fromList coordList
    print $ solve1 (map fst coordList) coordMap
    print $ solve2 (map fst coordList) coordMap

solve1 :: [Coord] -> Map.Map Coord Char -> Int
solve1 coords coordsMap =
    let clusters = evalState (getClusters coordsMap) (Set.fromList coords)
    in sum $ map price clusters

solve2 :: [Coord] -> Map.Map Coord Char -> Int
solve2 coords coordsMap =
    let clusters = evalState (getClusters coordsMap) (Set.fromList coords)
    in sum $ map price2 clusters

price2 :: Set.Set Coord -> Int
price2 coords = Set.size coords * (sum $ map (corners coords) (Set.toList coords))

corners :: Set.Set Coord -> Coord -> Int
corners region (x, y) =
    let n = neighbors (x, y)
        nRegion = filter (\c -> Set.member c region) n
        lineDistance (x1, y1) (x2, y2) = max (abs (x2-x1)) (abs (y2-y1))
    in  if length nRegion == 0 then 4 else
        if length nRegion == 1 then 2 else
        if length nRegion == 2 && lineDistance (nRegion !! 0) (nRegion !! 1) == 2 then 0 else
        if length nRegion == 2 then 1 + if getInsideCorner (x, y) (nRegion !! 0) (nRegion !! 1) `Set.member` region  then 0 else 1 else
        if length nRegion == 4 then length $ filter (\coord -> not $ coord `Set.member` region) (cornersCoords (x, y)) else
        if length nRegion == 3 then handleT (x, y) nRegion region else
        error $ "Found edge case " ++ show nRegion

handleT :: Coord -> [Coord] -> Set.Set Coord -> Int
handleT coord nRegion region =
    let bottomT = head $ filter (\(x, y) -> length ((filter (\(x', y') -> x' == x || y' == y)) nRegion) == 1) nRegion
        corners = cornersCoords coord
        cornerNeighbors = map (\c -> (c, neighbors c)) corners
        filtered = filter (\(c, ns) -> bottomT `elem` ns) cornerNeighbors
    in length $ filter (\(c, _) -> not $ c `Set.member` region) filtered


cornersCoords :: Coord -> [Coord]
cornersCoords (x, y) = [(x+1, y+1), (x-1, y+1), (x+1, y-1), (x-1, y-1)]

getInsideCorner :: Coord -> Coord -> Coord -> Coord
getInsideCorner coord n1 n2 =  -- Want the corner whose neighbors include both n1 and n2
    let corners = cornersCoords coord
        cornerNeighbors = map (\c -> (c, neighbors c)) corners
        filtered = filter (\(c, ns) -> n1 `elem` ns && n2 `elem` ns) cornerNeighbors
    in fst $ head filtered
        
price :: Set.Set Coord -> Int
price coords = Set.size coords * (sum $ map perimiter (Set.toList coords))
    where perimiter coord = 4 - (length $ filter (\c -> c `Set.member` coords) (neighbors coord))

getClusters :: Map.Map Coord Char -> State (Set.Set Coord) ([Set.Set Coord])
getClusters coordMap = do
    remaining <- get
    if Set.size remaining == 0 then return []
    else do
        let coord = Set.elemAt 0 remaining
        let cluster =  execState (getCluster coord (coordMap Map.! coord) coordMap) Set.empty
        let remaining' = remaining Set.\\ cluster
        put remaining'
        next <- getClusters coordMap
        return $ cluster : next
    
getCluster :: Coord -> Char -> Map.Map Coord Char -> State (Set.Set Coord) ()
getCluster coord char coordMap = do
    visited <- get
    if not (Map.member coord coordMap) || coordMap Map.! coord /= char || coord `Set.member` visited
        then return () 
        else do
            put $ Set.insert coord visited
            sequence_ $ map (\c -> getCluster c char coordMap) (neighbors coord)
