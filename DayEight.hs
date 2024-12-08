import DayZero
import Data.List

main :: IO ()
main = do
    (input, xMax, yMax) <- readAsMap "d8.txt"
    let filtered = filter (\i -> snd i /= '.') input
    print $ solve1 filtered xMax yMax
    print $ solve2 filtered xMax yMax


solve1 :: [(Coord, Char)] -> Int -> Int -> Int
solve1 = solve [1]

solve2 :: [(Coord, Char)] -> Int -> Int -> Int
solve2  = solve [0..100]

solve :: [Int] -> [(Coord, Char)] -> Int -> Int -> Int
solve ns input xMax yMax = 
    let grouped = groupBy (\x y -> snd x == snd y) (sortBy (\x y -> compare (snd x) (snd y)) input)
    in length $ nub $ filter (\(x, y) -> x <= xMax && y <= yMax && x >= 0 && y >= 0) (solve1' grouped)
        where solve1' (x:xs) =
                let coords = map fst x
                    coordPairs = pairs coords
                in (solve1' xs) ++ (concat $ map (\((x1, y1), (x2, y2)) ->  let (xDiff, yDiff) = (x2-x1, y2-y1) in [(x1-n*xDiff, y1-n*yDiff) | n <- ns] ++ [(x2+n*xDiff, y2+n*yDiff) | n <-ns]) coordPairs)
              solve1' [] = []

pairs :: [a] -> [(a, a)]
pairs l = [(x,y) | (x:ys) <- tails l, y <- ys]