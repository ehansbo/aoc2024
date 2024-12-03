import Text.Regex
import Data.Maybe (fromJust)
import Data.List.Split (splitOn)

mulRegex :: Regex
mulRegex = mkRegex "mul\\(([0-9]{1,3},[0-9]{1,3})\\)"

doRegex = mkRegex "do\\(\\)"
dontRegex = mkRegex "don't\\(\\)"

main :: IO ()
main = do
    input <- readFile "d3.txt"
    print $ solve1 input
    print $ solve2 input

solve2 :: String -> Int
solve2 str = 
    let cleaned = clean str
    in solve1 cleaned

clean :: String -> String
clean str = 
    let maybeFound = matchRegexAll dontRegex str
    in  case maybeFound of
            Nothing -> str
            Just (before, _, after, _) ->
                let maybeNextDo = matchRegexAll doRegex after
                in  case maybeNextDo of
                        Nothing -> before
                        Just (_, _, afterDo, _) -> before ++ clean afterDo

solve1 :: String -> Int
solve1 str =
    let maybeFound = matchRegexAll mulRegex str
    in  case maybeFound of 
            Just (_, _, rest, (res:[])) -> (readMul res) + (solve1 rest)
            Nothing -> 0

readMul :: String -> Int
readMul str =
    let i1:i2:[] = splitOn "," str
    in (read i1) * (read i2)