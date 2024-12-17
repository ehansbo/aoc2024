import DayZero
import qualified Control.Monad.State as S
import Data.Char (isDigit)
import Data.Bits (xor)

data Program = Program {a :: Int, b :: Int, c :: Int, pointer :: Int, input :: [Int], output :: [Int]}
    deriving (Show)

data Operand = Literal Int | Combo Int
data Register = A | B | C
data Operation = 
    Division Register Operand |
    XOR Operand |
    RXOR |
    Mod8 Operand |
    JumpNZ Operand |
    Out Operand

type PState = S.State Program


main :: IO ()
main = do
    rows <- splitFile "d17.txt" "\n"
    let program = parse rows
    let output = run program
    print output

    let run' = runA program
    let n = length (input program) - 1
    print $ solve2 run' (input program) n 0

runA :: Program -> Int -> [Int]
runA program i = run $ program {a = i}

solve2 :: (Int -> [Int]) -> [Int] -> Int -> Int -> Maybe Int
solve2 _ _ (-1) prev = Just prev
solve2 f ans n prev =
    let coeffs = [0..7]
        runs = zip coeffs (map (\c -> f (prev + c*8^n)) coeffs)
        correctOnes = map fst $ filter (\(x, y) -> length y == length ans && y !! n == ans !! n) runs
        belowRuns = filter (/= Nothing) $ map (\x -> solve2 f ans (n-1) (prev + x*8^n)) correctOnes
    in if length belowRuns == 0 then Nothing else (head belowRuns)


run :: Program -> [Int]
run program = S.evalState runS program

runS :: PState [Int]
runS = do
    operation <- getOperation
    shouldJump <- process operation
    program <- S.get
    let pointer' = if shouldJump then (pointer program + 2) else (pointer program)
    setPointer pointer'
    let i = input program
    if pointer' < 0 || pointer' >= length i then return $ output program else runS

process :: Operation -> PState Bool
process operation = do
    program <- S.get
    case operation of
        Division r o -> do
            numerator <- getReg A
            denomFac <- calcOperand o
            let denom = 2^denomFac
            insertReg r (numerator `div` denom)
            return True
        XOR o -> do
            val <- calcOperand o
            bVal <- getReg B
            insertReg B (val `xor` bVal)
            return True
        RXOR -> do
            cVal <- getReg C
            bVal <- getReg B
            insertReg B (bVal `xor` cVal)
            return True
        Mod8 o -> do
            val <- calcOperand o
            insertReg B (val `mod` 8)
            return True
        JumpNZ o -> do
            val <- calcOperand o
            aVal <- getReg A
            if aVal == 0 
                then return True
                else do
                    setPointer val
                    return False
        Out o -> do
            val <- calcOperand o
            let current = output program
            S.put $ program {output = current ++ [val `mod` 8]}
            return True


setPointer :: Int -> PState () 
setPointer i = do
    program <- S.get
    let inp = input program
    S.put $ program {pointer = i}


getReg :: Register -> PState Int
getReg r = do
    program <- S.get
    return $ case r of
        A -> a program
        B -> b program
        C -> c program

insertReg :: Register -> Int -> PState ()
insertReg r i = do
    program <- S.get
    case r of
        A -> S.put $ program {a = i}
        B -> S.put $ program {b = i}
        C -> S.put $ program {c = i}

calcOperand :: Operand -> PState Int
calcOperand (Literal i) = return i
calcOperand (Combo i) = do
    program <- S.get
    return $ case i of
        4 -> a program
        5 -> b program
        6 -> c program
        _ -> i


getOperation :: PState Operation
getOperation = do
    program <- S.get
    let i = input program
    let p = pointer program
    return $ parseOperation (i !! p) (i !! (p+1))

parseOperation :: Int -> Int -> Operation
parseOperation 0 i = Division A (Combo i)
parseOperation 1 i = XOR (Literal i)
parseOperation 2 i = Mod8 (Combo i)
parseOperation 3 i = JumpNZ (Literal i)
parseOperation 4 i = RXOR
parseOperation 5 i = Out (Combo i)
parseOperation 6 i = Division B (Combo i)
parseOperation 7 i = Division C (Combo i)


parse :: [String] -> Program
parse (aStr:bStr:cStr:pStr:_) =
    let dropped str = dropWhile (not . isDigit) str
        rd str = read $ dropped str
    in Program (rd aStr) (rd bStr) (rd cStr) 0 (read $ "[" ++ dropped pStr ++ "]") []

