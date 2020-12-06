import Zero.Zero
import Data.List
import Control.Arrow

main :: IO ()
main = do
   input <- lines <$> readFile "05.txt"
   print $ maximum $ bin <$> input
   print $ gaps $ bin <$> input

bin :: String -> Int
bin = foldl' go 0
   where
   go a d = 2 * a + b d
   b d
      | d ∈ "BR" = 1
      | d ∈ "FL" = 0
      | otherwise = error "no bin parse"

-- part 2

gaps :: (Ord a,Enum a) => [a] -> [a]
gaps = uncurry (go []) . (head &&& tail) . sort
   where
   go acc _ [] = acc
   go acc e (x:xs)
      | x == succ e = go acc x xs
      | otherwise = go (succ e : acc) (succ e) (x:xs)

