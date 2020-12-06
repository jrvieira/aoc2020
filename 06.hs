import Zero.Zero
import Data.List
import Data.List.Split
import Control.Monad

main :: IO ()
main = do
   input <- map (splitOn "\n") . splitOn "\n\n" . init <$> readFile "06.txt"
   print $ sum $ length . unique . join <$> input
   print $ sum $ length . foldr1 intersect . unique <$> input

