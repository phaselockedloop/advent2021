import Data.Char ()
import System.IO (isEOF)
import Text.Regex.TDFA ( (=~) )
import Text.Regex.TDFA.Text ()

main :: IO ()
main = multPosition (0, 0)

newPosition :: Maybe (String, Int) -> Int -> Int -> (Int, Int)
newPosition op cDepth cDist =
  case op of
    Just ("forward", change) -> (cDepth, cDist + change)
    Just ("down", change) -> (cDepth + change, cDist)
    Just ("up", change) -> (cDepth - change, cDist)
    _ -> (cDepth, cDist)

process :: String -> Maybe (String, Int)
process a = do
  let matches = a =~ "([a-zA-Z]+) ([0-9]+)" :: (String, String, String, [String])
  case matches of (_, _, _, [op, num]) -> Just (op, read num :: Int)
                  _ -> Nothing


multPosition :: (Int, Int) -> IO ()
multPosition (cDepth, cDist) = do
  done <- isEOF
  if done
    then print (cDepth * cDist)
    else do
      line <- getLine
      multPosition (newPosition (process line) cDepth cDist)