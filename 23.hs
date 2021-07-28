import Zero.Zero
import Data.Function
import Data.Char
import Data.List
import Data.List.Split
import Data.IntMap.Strict as M
import Control.Arrow

test :: IO ()
test = do
   let input = "389125467"
   teqt "part 1" "92658374" $ cups 10  input
   teqt "part 1" "67384529" $ cups 100 input
   teqt "part 2" 149245887792 $ cups' input

main :: IO ()
main = do
   let input = "792845136"
   print $ cups 100 input
   print $ cups' input

cups :: Int -> String -> String
cups n = fmap intToDigit . snip . (!! n) . iterate move . fmap digitToInt
   where
   snip ls = take (pred $ length ls) $ tail $ dropWhile (/= 1) $ cycle ls

move :: [Int] -> [Int]
move cs = bef <> [dest] <> pick <> aft <> [cur] -- # (intersperse ' ' $ intToDigit <$> cs) <> show (cur,pick,dest)
   where
   [bef,aft] = splitOn [dest] rest
   (cur:cups) = cs
   (pick,rest) = splitAt 3 cups
   dest = uncurry (flip maybe id) $ (head &&& find (< cur)) $ sortBy (flip compare) rest

-- part 2

{- OPTIMIZATION

   C - - - * * * * * *
         C * * d - - - * * * C

           C - - - * * * * * *
                 C * * * d - - - * * C

   C ->    *

     _ <- (C-1)


-}

cups' :: String -> Integer
cups' s = uncurry (on (*) toInteger) $ ((! 1) &&& uncurry (!) . (id &&& (! 1))) $ (!! 10000000) $ iterate move' $ M.fromList $ ((0,head cs) :) $ uncurry zip $ (id &&& tail . cycle) cs
   where
   cs = fmap digitToInt s <> [10..1000000]

move' :: M.IntMap Int -> M.IntMap Int
move' m = M.insert cur bef $ M.insert pick_end aft $ M.insert dest pick_start $ M.insert 0 bef $ m
   where
   cur = m ! 0
   pick_start = m ! cur
   pick_middle = m ! pick_start
   pick_end = m ! pick_middle
   bef = m ! pick_end
   dest = go (pred cur)
      where
      go x
         | x < 1 = go 1000000
         | x ∈ [pick_start,pick_middle,pick_end] = go (pred x)
         | otherwise = x
   aft = m ! dest

