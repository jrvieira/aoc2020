import Zero.Zero
import Data.List.Split
import qualified Data.Vector.Unboxed as V
import Control.Arrow

main :: IO ()
main = do
   input <- map read . splitOn "," <$> readFile "15.txt"
   tests <- map (map read . splitOn ",") . lines <$> readFile "15.test"
   teqt "part 1" [436,1,10,27,78,438,1836] $ solve 2020 <$> tests
   print $ solve 2020 input
   print $ solve 30000000 input

solve :: Int -> [Int] -> Int
solve n = uncurry go . (V.fromList . reverse &&& length)
   where
   go v i
      | i == n = V.head v
      | otherwise = go (V.cons x' v) (succ i)
      where
      x' = maybe 0 succ $ V.elemIndex (V.head v) (V.tail v)

