import DayZero
import Data.List.Split
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

data Puzzle = Puzzle {buttonA :: (Int, Int), buttonB :: (Int, Int), prize :: (Int, Int)}
    deriving (Show)

main :: IO ()
main = do
    input <- manuallyParse "d13.txt" "\n\n" parsePuzzle
    print $ solve1 input
    print $ solve1 (map increasePrize input)

increasePrize :: Puzzle -> Puzzle
increasePrize (Puzzle bA bB (x, y)) = Puzzle bA bB (x+10000000000000, y+10000000000000)

solve1 :: [Puzzle] -> Int
solve1 puzzles =
    let maybeSolutions = map maybeSolve puzzles
    in sum $ map (\(Just i) -> i) (filter (/= Nothing) maybeSolutions)

maybeSolve :: Puzzle -> Maybe Int
maybeSolve (Puzzle (xA, yA) (xB, yB) (xP, yP)) =
    let bPress = (yP*xA-yA*xP) `div` (yB*xA-yA*xB)
        aPress = (xP - xB * bPress) `div` xA
    in if bPress * xB + aPress * xA == xP && bPress * yB + aPress * yA == yP then (Just $ aPress * 3 + bPress) else Nothing

parsePuzzle :: String -> Puzzle
parsePuzzle str =
    let splitted = splitOn "\n" str
        strA = head splitted
        strB = splitted !! 1
        strPrize = splitted !! 2
    in Puzzle (parseButton strA) (parseButton strB) (parsePrize strPrize)

parseButton :: String -> (Int, Int)
parseButton str = 
    let x = read $ takeWhile isDigit (drop 12 str)
        y = read $ (drop 4 . dropWhile isDigit . drop 12) str
    in (x, y) 

parsePrize :: String -> (Int, Int)
parsePrize str =
    let x = read $ takeWhile isDigit (drop 9 str)
        y = read $ (drop 4 . dropWhile isDigit . drop 9) str
    in (x, y)

