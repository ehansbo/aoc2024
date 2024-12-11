import DayZero
import Data.List

type Count = Int

main :: IO ()
main = do
    input <- splitAndReadFile "d11.txt" " "
    print $ solve1 input
    print $ solve2 input

solve1 :: [Int] -> Int
solve1 input = solve input 25

solve2 :: [Int] -> Int
solve2 input = solve input 75

solve :: [Int] -> Int -> Int
solve input iterations = 
    let final = iterate run (zip input [1,1..]) !! iterations
    in sum $ map snd final

run :: [(Int, Count)] -> [(Int, Count)]
run input =
    let results = concat $ map run' input
        grouped = groupBy (\(x1, y1) (x2, y2) -> x1 == x2) (sortOn fst results)
    in map (\l -> (fst $ head l, sum (map snd l))) grouped
    where run' (x, i)
            | x == 0 = [(1, i)]
            | length (show x) `mod` 2 == 0 = 
                let half = length (show x) `div` 2
                    xStr = show x
                in [(read (take half xStr), i), (read (drop half xStr), i)]
            | otherwise = [(x*2024, i)]