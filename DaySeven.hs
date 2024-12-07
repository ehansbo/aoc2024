import DayZero
import Data.List.Split

type Calibration = (Int, [Int])

main :: IO ()
main = do
    input <- manuallyParse "d7.txt" "\n" parse
    print $ solve1 input
    print $ solve2 input

parse :: String -> Calibration
parse str = (read (takeWhile (/= ':') str), map read (tail $ splitOn " " str))

solve1 :: [Calibration] -> Int
solve1 = sum . map fst . filter valid1

solve2 :: [Calibration] -> Int
solve2 = sum . map fst . filter valid2

valid1 :: Calibration -> Bool
valid1 (solution, inputs) = valid' (head inputs) (tail inputs)
    where valid' acc (x:xs)
            | acc > solution = False
            | otherwise = valid' (acc+x) xs || valid' (acc*x) xs
          valid' acc [] = acc == solution

valid2 :: Calibration -> Bool
valid2 (solution, inputs) = valid' (head inputs) (tail inputs)
    where valid' acc (x:xs)
            | acc > solution = False
            | otherwise = valid' (acc+x) xs || valid' (acc*x) xs || valid' (read $ show acc ++ show x) xs
          valid' acc [] = acc == solution