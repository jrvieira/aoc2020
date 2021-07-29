import Zero.Zero
import Data.List
import Data.Bifunctor
import Control.Monad
import Control.Arrow hiding ( loop )

test :: IO ()
test = do
   let input = (5764801,17807724) -- public keys
   teqt "part 1" (8,11) $ join bimap loop input -- transform interations
   teqt "part 1" 14897079 $ solve input

main :: IO ()
main = do
   let input = (14205034,18047856)
   print $ join bimap loop input
   print $ solve input

solve :: (Integer,Integer) -> Integer
solve = τ . (loop . fst &&& snd)
   where
   τ (l,k) = transform k !! l

loop :: Integer -> Int
loop pub = fst $ maybe undefined id $ find ((== pub) . snd) $ zip [0..] $ transform 7

transform :: Integer -> [Integer]
transform subj = iterate go 1
   where
   go = flip rem 20201227 . (* subj)

