import Zero.Zero
import Data.List
import Data.List.Split

main :: IO ()
main = do
   input <- map (map (take 4) . splitOnAny [" ","\n"]) . splitOn "\n\n" <$> readFile "04.txt"
   print $ count True $ validate input

splitOnAny :: Eq a => [[a]] -> [a] -> [[a]]
splitOnAny ds xs = foldl' (\ys d -> ys >>= splitOn d) [xs] ds

fields = [
   "byr:",
   "iyr:",
   "eyr:",
   "hgt:",
   "hcl:",
   "ecl:",
   "pid:"
   ]

validate :: [[String]] -> [Bool]
validate = map valid
   where
   valid p = and $ ($ p) <$> (elem <$> fields)
