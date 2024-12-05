import DayZero
import Data.List.Split (splitOn)
import Data.List

type Order = (Int -> Int -> Ordering)

main :: IO ()
main = do
    (logicStr:inputStr:[]) <- splitFile "d5.txt" "\n\n"
    let ordering = getOrdering logicStr
    let inputs = map (map read . splitOn ",") (filter (/= "") (splitOn "\n" inputStr))
    print $ solve1 ordering inputs
    print $ solve2 ordering inputs

getOrdering :: String -> Order
getOrdering str = 
    let rules = map (\s -> (read $ take 2 s, read $ drop 3 s)) ((splitOn "\n") str)
        getOrdering' ((before, after):xs) x y = if before == x && after == y then LT else if before == y && after == x then GT else getOrdering' xs x y
        getOrdering' [] _ _ = EQ
    in getOrdering' rules


solve2 :: Order -> [[Int]] -> Int
solve2 = solve True

solve1 :: Order -> [[Int]] -> Int
solve1 = solve False

solve :: Bool -> Order -> [[Int]] -> Int
solve shouldChange order inputs =
    let solve' (i:is) = 
         let sorted = sortBy order i
         in if (sorted == i) == shouldChange then solve' is else sorted !! (length sorted `div` 2) + solve' is
        solve' [] = 0
    in solve' inputs
