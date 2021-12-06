import Data.Char ()
import System.IO (isEOF)
import Text.Regex.TDFA ((=~))
import Text.Regex.TDFA.Text ()

main :: IO ()
main = multPosition (0, 0, 0)

newPosition :: Maybe (String, Int) -> Int -> Int -> Int -> (Int, Int, Int)
newPosition op aim cDepth cDist =
  case op of
    Just ("forward", change) -> (aim, cDepth + (aim * change), cDist + change)
    Just ("down", change) -> (aim + change, cDepth, cDist)
    Just ("up", change) -> (aim - change, cDepth, cDist)
    _ -> (aim, cDepth, cDist)

process :: String -> Maybe (String, Int)
process a = do
  let matches = a =~ "([a-zA-Z]+) ([0-9]+)" :: (String, String, String, [String])
  case matches of
    (_, _, _, [op, num]) -> Just (op, read num :: Int)
    _ -> Nothing

multPosition :: (Int, Int, Int) -> IO ()
multPosition (aim, cDepth, cDist) = do
  done <- isEOF
  if done
    then print (cDepth * cDist)
    else do
      line <- getLine
      multPosition (newPosition (process line) aim cDepth cDist)