-- import Zero.Zero
import Data.List.Split
import Control.Arrow
import Control.Monad

parse :: String -> (Integer,[String])
parse = (read . head &&& join . map (splitOn ",") . tail) . lines

main :: IO ()
main = do
   input <- parse <$> readFile "13.txt"
-- tests <- map parse . splitOn "\n\n" <$> readFile "13.test"
-- teqt "part 1" 295 $ solve (head tests)
   print $ solve  input
-- teqt "part 2" [1068781,3417,754018,779210,1261476,1202161486] $ solve' <$> tests
   print $ solve' input

solve :: (Integer,[String]) -> Integer
solve (x,table) = uncurry (*) $ minimum $ ((rem x &&& id) . read) <$> filter (/= "x") table

-- part 2

type Bus = (Integer,Integer)

solve' :: (Integer,[String]) -> Integer
solve' (_,table) = f [ (read b,δ) | (δ,b) <- zip [0..] table , b /= "x" ]

f :: [Bus] -> Integer
f = go 1 0
   where
   go _ τ [] = τ
   go step τ β@((b,δ):bs)
      | rem (τ + δ) b == 0 = go (lcm step b) τ bs  -- lcm can be (*) , all buses are prime
      | otherwise = go step (τ + step) β

