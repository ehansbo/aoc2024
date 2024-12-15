module DayZero where
import Data.List.Split
import Data.List

data Direction = North | South | West | East
    deriving (Show, Eq)

type Coord = (Int, Int)

(!?) :: [a] -> Int -> Maybe a
[]     !? n = Nothing
(a:as) !? 0 = Just a
(a:as) !? n = as !? (n-1)


splitAndReadFile :: Read a => String -> String -> IO [a]
splitAndReadFile name splitter = do
        input <- splitFile name splitter
        return $ map read input

splitTwiceAndRead :: Read a => String -> String -> String -> IO [[a]]
splitTwiceAndRead name bigSplit smallSplit = do
    input <- splitFile name bigSplit
    return $ map (\l -> map read (splitOn smallSplit l)) input

manuallyParse :: String -> String -> (String -> a) -> IO [a]
manuallyParse name splitter f = do
    input <- splitFile name splitter
    return $ map f input

splitFile :: String -> String -> IO [String]
splitFile name splitter = do
    input <- readFile name
    return $ filter (\x -> x /= "") $ splitOn splitter input


uniq :: (Ord a) => [a] -> [a]
uniq = map head . group . sort

isNumber :: Char -> Bool
isNumber c = c `elem` (['0'..'9'] ++ "-")

readAsMap :: String -> IO ([(Coord, Char)], Int, Int)
readAsMap name = do
    input <- splitFile name "\n"
    return $ getCoords input

getCoords :: [String] -> ([(Coord, Char)], Int, Int)
getCoords rows =
    let zipped = zip [0..] (map (zip [0..]) rows)
    in (concat $ map (\(y, row) -> map (\(x, c) -> ((x, y), c)) row) zipped, length (head rows)-1, length rows-1)

neighbors :: Coord -> [Coord]
neighbors (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]

nextPos :: Direction -> Coord -> Coord
nextPos East (x, y) = (x+1, y)
nextPos West (x, y) = (x-1, y)
nextPos North (x, y) = (x, y-1)
nextPos South (x, y) = (x, y+1)