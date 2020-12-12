import Zero.Zero
import Util
import Data.List.Split
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Control.Monad

main :: IO ()
main = do
   input <- ferry <$> readFile "11.txt"
   tests <- map ferry . splitOn "\n\n"  <$> readFile "11.test"
   teqt "part 1" 37 $ solve (tests !! 0)
   print $ solve $ input

type Ferry = V.Vector (U.Vector Char)

ferry :: String -> Ferry
ferry = V.fromList . map U.fromList . lines

(><) :: Ferry -> (Int,Int) -> Maybe Char
f >< (x,y) = join fu
   where
   fv = f V.!? x
   fu = (U.!? y) <$> fv

solve :: Ferry -> Int
solve = count (== '#') . join . map U.toList . V.toList . fixp φ
   where
   φ :: Ferry -> Ferry
   φ v = V.imap φ' v
      where
      φ' x u = U.imap φ'' u
         where
         φ'' y c
            | 'L' <- c , adjacents '#' == 0 = '#'
            | '#' <- c , adjacents '#' >= 4 = 'L'
            | otherwise = c
            where
            adjacents chr = count (Just chr ==) $ map (v ><) [(x',y') | x' <- [x-1..x+1] , y' <- [y-1..y+1] , (x',y') /= (x,y) ]

