import DayZero
import Data.List.Split (splitOn)
import Data.List (intercalate)
import Data.Char (isDigit)
import qualified Data.Set as S

data Robot = Robot {pos :: Coord, vel :: (Int, Int)}
    deriving (Show)
width = 101
height = 103

main :: IO ()
main = do
    input <- manuallyParse "d14.txt" "\n" parse
    print $ solve1 input
    let zipped = zip [27, 128..10000] (map (\s -> map (move s) input) [27, 128..10000]) -- the numbers here were found manually - every 101 outputs, starting at 27, had a "tall" pattern, so iterated over those to find the tree.
    mapM_ putStrLn (map toString zipped)

toString :: (Int, [Robot]) -> String
toString (i, robots) = show i ++ "\n" ++ intercalate "\n" (printGrid (S.fromList (map pos robots)))
    where printGrid robotSet = do
            y <- [0..height-1]
            return $ do
                x <- [0..width-1]
                return $ if (x, y) `S.member` robotSet  then 'O' else '.'

--solve1 :: [Robot] -> Int
solve1 robots = 
    let robots' = map (move 100) robots
        byQuadrant xMin xMax yMin yMax (Robot (x, y) _) = x >= xMin && x < xMax && y >= yMin && y < yMax
        bq1 = filter (byQuadrant 0 ((width - 1 ) `div` 2) 0 ((height - 1) `div` 2)) robots'
        bq2 = filter (byQuadrant ((width + 1) `div` 2) width 0 ((height - 1) `div` 2)) robots'
        bq3 = filter (byQuadrant 0 ((width - 1 ) `div` 2) ((height + 1) `div` 2) height) robots'
        bq4 = filter (byQuadrant ((width + 1) `div` 2) width ((height + 1) `div` 2) height) robots'
    in product [length bq1, length bq2, length bq3, length bq4]

move :: Int -> Robot -> Robot
move i (Robot (pX, pY) (vX, vY)) = Robot ((pX+vX*i) `mod` width, (pY+vY*i) `mod` height) (vX, vY)

parse :: String -> Robot
parse str = 
    let (pStr:vStr:[]) = splitOn " " str
        parse' str' = read $ "(" ++ dropWhile (\c -> not (isDigit c) && c /= '-') str' ++ ")"
    in Robot (parse' pStr) (parse' vStr)