import Data.Char(digitToInt)
import Data.List


data Layout = Id Int | Free
    deriving (Eq, Ord)

instance Show Layout where
    show (Id i) = show i
    show Free = "."

main :: IO ()
main = do
    input <- readInput "d9.txt"
    print $ solve1 input
    print $ solve2 input

solve1 :: [Int] -> Int
solve1 input =
    let zipped = zip (interleave [Id x | x <- [0..]] (repeat Free)) input 
        files = concat $ map (\(x, n) -> take n (repeat x)) zipped
        reversed = reverse $ filter (/= Free) files
        moved = move files reversed (length reversed)
    in sum $ map (\((Id i), n) -> i*n) (zip moved [0..])

solve2 :: [Int] -> Int
solve2 input =
    let zipped = zip (interleave [Id x | x <- [0..]] (repeat Free)) input 
        files = filter (/= []) (map (\(x, n) -> take n (repeat x)) zipped)
        moved = concat $ reverse $ move2 $ reverse files
    in sum $ map (\(l, n) -> (getScore l*n)) (zip moved [0..])

move2 :: [[Layout]] -> [[Layout]]
move2 (x:xs)
    | head x == Free = x : move2 xs
    | otherwise =
        let reversed = reverse xs
            (before, after) = span (\x' -> head x' /= Free || length x' < length x) reversed
        in  if length after == 0 then x : move2 xs else
                let remainingFree = drop (length x) (head after)
                in move2 $ filter (/= []) $ reverse $ before ++ x : remainingFree : tail after ++ [(take (length x) (repeat Free))]
move2 [] = []

move :: [Layout] -> [Layout] -> Int -> [Layout]
move _ _ 0 = []
move (Free:xs) ((Id i):ys) remaining = (Id i): move xs ys (remaining - 1)
move ((Id i):xs) ys remaining = Id i : move xs ys (remaining - 1)

getId (Id i) = i

getScore (Id i) = i
getScore Free = 0

readInput :: String -> IO [Int]
readInput fileName = do
    input <- readFile fileName
    return $ map digitToInt (filter (/= '\n') input)

interleave :: [a] -> [a] -> [a]
interleave a b = concat $ transpose [a, b]
