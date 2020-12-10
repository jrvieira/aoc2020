import Zero.Zero

main :: IO ()
main = do
   input <- map read . lines <$> readFile "09.txt"
-- test1 <- map read . lines <$> readFile "09.test"
-- teqt "part 1" 127 $ solve 5 test1
   print $ solve 25 input

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
fsum t p = go $ foldr (:) [] p
   where
   go [] = False
   go (x:xs)
      | t > x && t /= 2 * x && (t - x) ∈ p = True
      | otherwise = go xs
