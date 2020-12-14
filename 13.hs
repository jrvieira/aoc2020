import Zero.Zero
import Data.List.Split
import Control.Arrow
import Control.Monad

main :: IO ()
main = do
   input <- (read . head &&& join . map (splitOn ",") . tail) . lines <$> readFile "13.txt"
   test <- (read . head &&& join . map (splitOn ",") . tail) . lines <$> readFile "13.test"
   teqt "part 1" 295 $ solve test
   print $ solve input

solve :: (Int,[String]) -> Int
solve (x,table) = uncurry (*) $ minimum $ zip (f <$> buses) buses
   where
   buses = map read $ filter (/= "x") table
   f b = b - (rem x b)
