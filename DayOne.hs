import DayZero
import Data.List.Split
import Data.List

main :: IO ()
main = do
    input <- manuallyParse "d1.txt" "\n" parser
    print $ solve1 input
    print $ solve2 input

solve1 :: [(Int, Int)] -> Int
solve1 xs =
    let (list1, list2) = (map fst xs, map snd xs)
        pairsSorted = zip (sort list1) (sort list2)
    in sum $ map (abs . (uncurry (-))) pairsSorted

solve2 :: [(Int, Int)] -> Int
solve2 xs = 
    let (list1, list2) = (map fst xs, map snd xs)
    in similarities list1 list2

similarities :: [Int] -> [Int] -> Int
similarities (x:xs) ys =
    x * (length $ filter (==x) ys) + similarities xs ys
similarities [] _ = 0

parser :: String -> (Int, Int)
parser str = 
    let s1:s2:[] = splitOn "   " str
    in (read s1, read s2)