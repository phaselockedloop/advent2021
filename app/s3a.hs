import Data.Char ()
import System.IO (isEOF)

main :: IO ()
main = calcPower [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]

combine :: String -> [Int] -> [Int]
combine add cur = zipWith (+) cur (map (\r -> read [r] :: Int) add)

gamma :: Int -> Int
gamma a = if a > 500 then 1 else 0

epi :: Int -> Int
epi a = if a <= 500 then 1 else 0

gammaEpi :: (Int -> Int) -> [Int] -> Int
gammaEpi f [] = 0
gammaEpi f (head : tail) = f head + (2 * gammaEpi f tail)

calcPower :: [Int] -> IO ()
calcPower counts = do
  done <- isEOF
  if done
    then do
        let r = reverse counts
        print (gammaEpi gamma r * gammaEpi epi r)
    else do
      line <- getLine
      calcPower (combine line counts)