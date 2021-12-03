import Data.Char ()
import System.IO (isEOF)

main :: IO ()
main = myLoop 0 (maxBound :: Int)

bumpTotal :: Int -> Int -> Int -> Int
bumpTotal total current previous = total + if current > previous then 1 else 0

myLoop :: Int -> Int -> IO ()
myLoop total previous = do
  done <- isEOF
  if done then print total
    else getLine >>= (\current -> myLoop (bumpTotal total current previous) current) . (\l -> read l :: Int)