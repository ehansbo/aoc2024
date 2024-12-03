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
    putStrLn input
    print $ solve1 input
    print $ solve2 input

solve2 :: String -> Int
solve2 = solve2' True
    where solve2' active str =
            let regex1 = if active then dontRegex else doRegex
                maybeFoundMul = matchRegexAll mulRegex str
                maybeFoundFlip = matchRegexAll regex1 str
                continue res rest = (if active then readMul res else 0 ) + solve2' active rest
            in  case maybeFoundMul of
                    Nothing -> 0
                    Just (beforeMul, _, restMul, (res:[])) ->
                        case maybeFoundFlip of 
                            Nothing -> continue res restMul
                            Just (beforeFlip, _, restFlip, _) -> 
                                if (length beforeFlip < length beforeMul) 
                                    then solve2' (not active) restFlip 
                                    else continue res restMul


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