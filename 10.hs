import Zero.Zero
import Data.List.Split

main :: IO ()
main = do
   input <- map read . lines <$> readFile "10.txt"
   tests <- map (read <$>) . splitOn [""] . lines <$> readFile "10.test"
   teqt "part 1" [11,31] $ solve <$> tests
   print $ solve input

solve :: [Int] -> Int
solve = const 9

