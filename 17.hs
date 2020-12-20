import Zero.Zero
import Linear.V3
import Linear.V4
import Data.Set (Set)
import qualified Data.Set as S

main :: IO ()
main = do
   input <- readFile "17.txt"
   test <- readFile "17.test"
   teqt "part 1" 112 $ solve $ parse test
   print $ solve  $ parse  input
   teqt "part 2" 848 $ solve' $ parse' test
   print $ solve' $ parse' input

parse :: String -> Set (V3 Int)
parse = go S.empty (0,0)
   where
   go :: Set (V3 Int) -> (Int,Int) -> String -> Set (V3 Int)
   go s _ "" = s
   go s (x,y) (p:ps)
      | '#' <- p = go (S.insert (V3 x y 0) s) (succ x,y) ps
      | '.' <- p = go s (succ x,y) ps
      | '\n' <- p = go s (0,succ y) ps
      | otherwise = error "no parse"

solve :: Set (V3 Int) -> Int
solve = S.size . (!! 6) . sim

sim :: (Applicative f, Num a, Enum a, Num (f a), Ord (f a), Traversable f) => Set (f a) -> [Set (f a)]
sim = iterate step

step s = S.filter alive s'
   where
   s' = foldl1 (∪) (S.map adjacents s) ∪ s
   alive x
      | x ∈ s = S.size (S.filter (∈ s) (adjacents x)) ∈ [2,3] -- S23
      | otherwise = S.size (S.filter (∈ s) (adjacents x)) ∈ [3] -- B3

adjacents v = S.fromList [ v + δ | δ <- sequence $ pure [-1..1] , δ /= pure 0 ]

-- part 2

parse' :: String -> Set (V4 Int)
parse' = go S.empty (0,0)
   where
   go :: Set (V4 Int) -> (Int,Int) -> String -> Set (V4 Int)
   go s _ "" = s
   go s (x,y) (p:ps)
      | '#' <- p = go (S.insert (V4 x y 0 0) s) (succ x,y) ps
      | '.' <- p = go s (succ x,y) ps
      | '\n' <- p = go s (0,succ y) ps
      | otherwise = error "no parse"

solve' :: Set (V4 Int) -> Int
solve' = S.size . (!! 6) . sim

