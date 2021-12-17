import           Data.Char ()
import           System.IO (isEOF)

-- Generic utility functions
count :: (a -> Bool) -> [a] -> Int
count pred = length . filter pred

binToNum :: [Int] -> Int
binToNum = foldl (\x y -> 2*x + y) 0

strToBin :: String -> [Int]
strToBin = map (\ l -> read (l : "") :: Int)

-- Problem specific utility functions
countOnes :: Int -> [[Int]] -> Int
countOnes pos = count (\val -> val !! pos == 1)

checkMatch :: Int -> Int -> [Int] -> Bool
checkMatch value pos input = input !! pos == value

-- IO Crap
readInput :: [[Int]] -> IO [[Int]]
readInput a = do
    done <- isEOF
    if done then do
     return a
    else do
      line <- getLine
      readInput (strToBin line : a)

-- Main execution
main :: IO ()
main = do
    r <- readInput []
    let ox = calcLifeSupport True 0 r
    let scrub = calcLifeSupport False 0 r
    print(binToNum ox * binToNum scrub)

-- Not safe at all and would fail on duplicates
calcLifeSupport :: Bool -> Int -> [[Int]] -> [Int]
calcLifeSupport isMajority pos i = do
    case i of
        [h] -> h
        [] -> []
        (h : t) -> do
            let ones = countOnes pos i
            let zeros = length i - ones
            let f = if isMajority == (ones >= zeros) then 1 else 0
            let result = filter (checkMatch f pos) i
            calcLifeSupport isMajority (pos + 1) result
