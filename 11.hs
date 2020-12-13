import Zero.Zero
import Util
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Control.Monad

main :: IO ()
main = do
   input <- ferry <$> readFile "11.txt"
   test <- ferry <$> readFile "11.test"
   teqt "part 1" 37 $ solve  test
-- print $ solve  input
   teqt "part 2" 26 $ solve' test
   print $ solve' input

type Ferry = V.Vector (U.Vector Char)

ferry :: String -> Ferry
ferry = V.fromList . map U.fromList . lines

(><) :: Ferry -> (Int,Int) -> Maybe Char
f >< (x,y) = join fu
   where
   fv = f V.!? y
   fu = (U.!? x) <$> fv

solve :: Ferry -> Int
solve = count (== '#') . join . map U.toList . V.toList . fixp φ
   where
   φ :: Ferry -> Ferry
   φ v = V.imap φ' v
      where
      φ' y u = U.imap φ'' u
         where
         φ'' x c
            | 'L' <- c , adjacents == 0 = '#'
            | '#' <- c , adjacents >= 4 = 'L'
            | otherwise = c
            where
            adjacents = count (Just '#' ==) $ (v ><) <$> [ (x',y') | x' <- [x-1..x+1] , y' <- [y-1..y+1] , (x',y') /= (x,y) ]

-- part 2

solve' :: Ferry -> Int
solve' = count (== '#') . join . map U.toList . V.toList . fixp φ
   where
   φ :: Ferry -> Ferry
   φ v = V.imap φ' v
      where
      φ' y u = U.imap φ'' u
         where
         φ'' x c
            | 'L' <- c , adjacents == 0 = '#'
            | '#' <- c , adjacents >= 5 = 'L'
            | otherwise = c
            where
            adjacents = sum $ look (x,y) <$> [ (x',y') | x' <- [-1..1] , y' <- [-1..1] , (x',y') /= (0,0) ]
            look (x,y) (xi,yi)
               | Just '#' <- sight = 1
               | Just '.' <- sight = look (x',y') (xi,yi)
               | otherwise = 0
               where
               sight = v >< (x',y')
               (x',y') = (x+xi,y+yi)

