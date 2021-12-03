import Data.Char ()
import System.IO (isEOF)

main :: IO ()
main = myLoop 0 Nothing Nothing Nothing

bumpTotal :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> Int -> Int
bumpTotal total (Just first) (Just second) (Just third) current =
  total + if (first + second + third) < (second + third + current) then 1 else 0
bumpTotal total _ _ _ _ = total

myLoop :: Int -> Maybe Int -> Maybe Int -> Maybe Int -> IO ()
myLoop total first second third = do
  done <- isEOF
  if done
    then print total
    else getLine >>= (\current -> myLoop (bumpTotal total first second third current) second third (Just current)) . (\l -> read l :: Int)