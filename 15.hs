-- import Zero.Zero
import Data.List.Split
import qualified Data.IntMap.Strict as M
import Control.Arrow

main :: IO ()
main = do
   input <- map read . splitOn "," <$> readFile "15.txt"
-- tests <- map (map read . splitOn ",") . lines <$> readFile "15.test"
-- teqt "part 1" [436,1,10,27,78,438,1836] $ solve 2020 <$> tests
   print $ solve 2020 input
   print $ solve 30000000 input

solve :: Int -> [Int] -> Int
solve n = uncurry go . (M.fromList &&& last) . flip zip [1..]
   where
   go m (x,i)
      | i == n = x
      | otherwise = go m' (x',i')
      where
      m' = M.insert x i m
      x' = maybe 0 (i -) $ M.lookup x m
      i' = succ i

