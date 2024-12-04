import DayZero
import Data.List

main :: IO ()
main = do
    rows <- splitFile "d4.txt" "\n"
    print $ solve1 rows
    print $ solve2 rows


solve1 :: [String] -> Int
solve1 rows =
    let coords = getCoordinates rows
        runCoord ((x1,y1):(x2,y2):(x3,y3):(x4,y4):[]) = rows ! (x1, y1) : rows ! (x2, y2) : rows ! (x3, y3) : rows ! (x4, y4) : []
    in length $ filter (\coords' -> runCoord coords' == "XMAS") coords

solve2 :: [String] -> Int
solve2 rows =
    let coordsA = filter (\(x, y) -> rows ! (x, y) == 'A') [(x, y) | x <- [1..length (head rows)-2], y <- [1..length rows-2]]
        crossMas (x, y) = 
            (rows ! (x-1, y-1) == 'M' && rows ! (x+1, y+1) == 'S' || rows ! (x-1, y-1) == 'S' && rows ! (x+1, y+1) == 'M') &&
            (rows ! (x-1, y+1) == 'M' && rows ! (x+1, y-1) == 'S' || rows ! (x-1, y+1) == 'S' && rows ! (x+1, y-1) == 'M')
    in length $ filter crossMas coordsA

(!) :: [[a]] -> (Int, Int) -> a
r ! (x, y) = (r !! y) !! x

getCoordinates :: [[a]] -> [[(Int, Int)]]
getCoordinates rows = 
    let xl = length (head rows)
        yl = length rows 
    in  [[(x, y), (x+1, y), (x+2, y), (x+3, y)] | x <- [0..xl-4], y <- [0..yl-1]] 
     ++ [[(x, y), (x-1, y), (x-2, y), (x-3, y)] | x <- [3..xl-1], y <- [0..yl-1]]
     ++ [[(x, y), (x, y-1), (x, y-2), (x, y-3)] | x <- [0..xl-1], y <- [3..yl-1]]
     ++ [[(x, y), (x, y+1), (x, y+2), (x, y+3)] | x <- [0..xl-1], y <- [0..yl-4]]
     ++ [[(x, y), (x+1, y+1), (x+2, y+2), (x+3, y+3)] | x <- [0..xl-4], y <- [0..yl-4]]
     ++ [[(x, y), (x-1, y-1), (x-2, y-2), (x-3, y-3)] | x <- [3..xl-1], y <- [3..yl-1]]
     ++ [[(x, y), (x-1, y+1), (x-2, y+2), (x-3, y+3)] | x <- [3..xl-1], y <- [0..yl-4]]
     ++ [[(x, y), (x+1, y-1), (x+2, y-2), (x+3, y-3)] | x <- [0..xl-4], y <- [3..yl-1]]