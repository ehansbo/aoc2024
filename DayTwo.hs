import DayZero

main :: IO ()
main = do
    input <- splitTwiceAndRead "d2.txt" "\n" " " 
    print $ solve1 input
    print $ solve2 input

solve1 :: [[Int]] -> Int
solve1 xs = length $ filter valid xs

solve2 :: [[Int]] -> Int
solve2 (xs:xss) = 
    let alternatives = xs : map (\i -> fst (splitAt i xs) ++ tail (snd (splitAt i xs))) [0..length xs-1]
    in if foldl1 (||) (map valid alternatives) then 1 + solve2 xss else 0 + solve2 xss
solve2 _ = 0

valid :: [Int] -> Bool
valid xs = 
    let getDiffs (y:z:ys) = z-y : getDiffs (z:ys)
        getDiffs _ = []
        diffs = getDiffs xs
        cmpDiffs f = foldl1 (&&) (map f diffs)
    in (cmpDiffs (<0) || cmpDiffs (>0)) && cmpDiffs ((<4) . abs)
