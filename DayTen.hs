import DayZero
import qualified Data.Map as Map
import Data.Char(digitToInt)
import Data.List (nub)

main :: IO ()
main = do
    input <- (\(i, _, _) -> map (\(a, b) -> (a, digitToInt b)) i) <$> readAsMap "d10.txt"
    let zeroCoords = map fst (filter (\s -> snd s == 0) input)
    print $ solve1 zeroCoords (Map.fromList input)
    print $ solve2  zeroCoords (Map.fromList input)


solve1 :: [Coord] -> Map.Map Coord Int -> Int
solve1 (x:xs) coordMap = solve1 xs coordMap + (length (nub $ nineCoords x))
    where nineCoords current =
            let i = coordMap Map.! current
                possibleNeighbors = filter (\c -> Map.findWithDefault 0 c coordMap == i + 1) (neighbors current)
            in if i == 9 then [current] else concat $ map nineCoords possibleNeighbors
solve1 _ _ = 0


solve2 :: [Coord] -> Map.Map Coord Int -> Int
solve2 (x:xs) coordMap = solve2 xs coordMap + nineCoords x
    where nineCoords current =
            let i = coordMap Map.! current
                possibleNeighbors = filter (\c -> Map.findWithDefault 0 c coordMap == i + 1) (neighbors current)
            in if i == 9 then 1 else sum $ map nineCoords possibleNeighbors
solve2 _ _ = 0

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]