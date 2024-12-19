import DayZero
import Data.List.Split (splitOn)
import Data.List (sortBy)
import Data.Function.Memoize

main :: IO ()
main = do
    (patternStr:designs) <- splitFile "d19.txt" "\n"
    let patterns = sortBy (\x y -> (compare (length y) (length x))) $ splitOn ", " patternStr
    print $ solve1 patterns designs
    print $ solve2 patterns designs

solve1 :: [String] -> [String] -> Int
solve1 patterns designs =
    let numberOfSolutions = map (run patterns) designs
    in length $ filter (> 0) numberOfSolutions

solve2 :: [String] -> [String] -> Int
solve2 patterns designs =
    let numberOfSolutions = map (run patterns) designs
    in sum numberOfSolutions

run :: [String] -> String -> Int
run = memoize2 run'
    where run' patterns [] = 1
          run' patterns remaining =
            let matching = filter (\p -> take (length p) remaining == p) patterns
            in if length matching == 0 then 0 else
                sum $ map (\m -> run patterns (drop (length m) remaining)) matching