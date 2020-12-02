import Data.List.Split
import Control.Arrow
import Data.Bool.Unicode

main :: IO ()
main = do
   list <- lines <$> readFile "02.txt"
   print . length . filter id . map validate $ list
   print . length . filter id . map validate' $ list

data Input = Input Int Int Char String

parse :: String -> Input
parse s = Input (read i1) (read i2) (head c) pw
   where
   [i1,rest] = splitOn "-" s
   [i2,c,pw] = splitOn " " rest

validate :: String -> Bool
validate s = uncurry (∧) . ((>= mn) &&& (<= mx)) . length . filter (== ch) $ pw
   where
   Input mn mx ch pw = parse s

validate' :: String -> Bool
validate' s = check i1 ⊻ check i2
   where
   check i
      | i > length pw = False
      | otherwise = pw !! (i - 1) == ch
   Input i1 i2 ch pw = parse s

