import Prelude hiding ((||))
import System.Environment (getArgs)
import System.Exit (die)

data InputLine = InputLine
    { testValue :: Integer
    , numbers :: [Integer]
    } deriving (Show)

parseLine :: String -> InputLine
parseLine line = 
    let (testValue, rest) = break (== ':') line
        nums = words $ tail rest
    in InputLine (read testValue) (map read nums)

parseInput :: String -> [InputLine]
parseInput = map parseLine . lines

readInput :: String -> IO [InputLine]
readInput filename = do
    content <- readFile filename
    return $ parseInput content

(||) :: Integer -> Integer -> Integer
a || b = read (show a ++ show b)

possibleCalibrations :: [Integer -> Integer -> Integer] -> [Integer] -> [Integer]
possibleCalibrations _ [a] = [a]
possibleCalibrations ops (a:b:rest) =
    concat [possibleCalibrations ops (op a b:rest) | op <- ops]

isValid :: [Integer -> Integer -> Integer] -> InputLine -> Bool
isValid ops (InputLine testValue nums) = testValue `elem` possibleCalibrations ops nums

part1Ops = [(+), (*)]
part2Ops = [(+), (||), (*)]

main :: IO ()
main = do
    args <- getArgs
    if length args /= 1
    then die "Usage: program <input-file>"
    else do
        content <- readInput $ head args
        -- mapM_ print content
        print $ sum $ [testValue line | line <- content, isValid part1Ops line]
        print $ sum $ [testValue line | line <- content, isValid part2Ops line]
