import Data.List
import Data.List.Split

main :: IO ()
main = do
   input <- map lines . splitOn "\n\n" <$> readFile "06.txt"
   print $ sum $ length . foldr1 union <$> input
   print $ sum $ length . foldr1 intersect <$> input

