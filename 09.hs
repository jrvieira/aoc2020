import Zero.Zero
import Data.Foldable
import Data.List

main :: IO ()
main = do
   input <- map read . lines <$> readFile "09.txt"
-- test1 <- map read . lines <$> readFile "09.test"
-- teqt "part 1" 127 $ solve 5 test1
   print $ solve  25 input
   print $ solve' (solve 25 input) input

solve :: Int -> [Int] -> Int
solve p l = go (queue $ take p l) (drop p l)
   where
   go _ [] = error "invulnerable"
   go q (x:xs)
      | fsum x q = go (step x q) xs
      | otherwise = x
   step x = snd . dequeue . enqueue x

-- recycled from day 01
fsum :: Int -> Queue Int -> Bool
fsum t q = go $ toList q
   where
   go [] = False
   go (x:xs)
      | t > x , t /= 2 * x , (t - x) ∈ q = True
      | otherwise = go xs

-- part 2

solve' :: Int -> [Int] -> Int
solve' t l = go [] $ reverse $ tails l
   where
   go :: [Int] -> [[Int]] -> Int
   go _ [] = error "invulnerable"
   go _ ([]:ls) = go [] ls
   go pre ((x:xs):ls)
      | length pre > 1 , sum pre == t = minimum pre + maximum pre
      | otherwise = go (x : pre) (xs:ls)

