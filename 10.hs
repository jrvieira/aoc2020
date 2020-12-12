-- import Zero.Zero
import Util
import Data.List
-- import Data.List.Split
import Control.Arrow

main :: IO ()
main = do
   input <- map read . lines <$> readFile "10.txt"
-- tests <- map (read <$>) . splitOn [""] . lines <$> readFile "10.test"
-- teqt "part 1" [35,220] $ solve <$> tests
   print $ solve  input
-- teqt "part 2" [8,19208] $ solve' <$> tests
   print $ solve' input

solve :: [Int] -> Int
solve = uncurry (*) . (count (== 3) &&& count (== 1)) . deltas

deltas :: [Int] -> [Int]
deltas = δ . (0 :) . uncurry (flip (++)) . (pure . (+ 3) . last &&& id) . sort
   where
   δ [] = error "deltas []"
   δ [_] = []
   δ (x:xx:xs) = abs (x - xx) : δ (xx : xs)

-- part 2

solve' :: [Int] -> Integer
solve' = go [] . reverse . tails . edges . deltas
   where
   go _ [] = 0
   go m (x:xs)
      | null xs = solve x
      | otherwise = go (solve x : m) xs
      where
      solve [] = 1
      solve (x:_) = sum $ (m !!) <$> [0..pred x]

edges :: [Int] -> [Int]
edges [] = []
edges a@(1:1:1:_) = 3 : edges (tail a)
edges a@(1:1:_) = 2 : edges (tail a)
edges a@(1:_) = 1 : edges (tail a)
edges a = 1 : edges (tail a)

