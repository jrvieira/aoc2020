import Zero.Zero
import Util
import Data.List
import Data.List.Split
import Control.Arrow

main :: IO ()
main = do
   input <- map read . lines <$> readFile "10.txt"
   tests <- map (read <$>) . splitOn [""] . lines <$> readFile "10.test"
   teqt "part 1" [35,220] $ solve <$> tests
   print $ solve  input
   teqt "part 2" [8,19208] $ solve' <$> tests

   print $ solve' input
-- teqt "on" [8,19208] $ map (on . deltas) tests
-- print $ on $ deltas $ input
-- print $ solve' input

solve :: [Int] -> Int
solve = uncurry (*) . (count (== 3) &&& count (== 1)) . deltas

deltas :: [Int] -> [Int]
deltas = δ . (0 :) . uncurry (flip (++)) . (pure . (+ 3) . last &&& id) . sort
   where
   δ [] = error "deltas []"
   δ [_] = []
   δ (x:xx:xs) = abs (x - xx) : δ (xx : xs)

-- part 2

solve' :: [Int] -> Int
solve' = go . edges . deltas
   where
   go :: [Int] -> Int
   go [] = 0
   go [x] = x
   go (x:xs) = sum $ map go $ map (tails xs ><) [0..pred x]
   xs >< n
      | length xs == n = pure 1
      | otherwise = xs !! n

edges :: [Int] -> [Int]
edges [] = []
edges a@(1:1:1:_) = 3 : edges (tail a)
edges a@(1:1:_) = 2 : edges (tail a)
edges a@(1:_) = 1 : edges (tail a)
edges a = 1 : edges (tail a)

-- on :: [Int] -> Integer
-- on [] = 0
-- on [x] = o x
-- on (x:xs) = (o x +) $ sum $ map on $ map snd $ takeWhile compatible $ zip [3,2..] $ init $ tail $ tails xs
--    where
--    compatible (d,ds) = d >= head ds 

-- on [] = 0
-- on [w] = o w 
-- on a@[w,_] = o w + on (tail a)
-- on a@[w,x,y]
--    | x == 3 = o w * on (tail a)
--    | y == 1 = o w * 3 + on (tail a)
--    | x == 1 = o w * on (tail a)
--    | otherwise = on (tail a)
-- on a@(w:x:y:z:_)
--    | z == 3 = o w * 3 + on (tail a)
--    | y == 1 = o w * 2 + on (tail a)
--    | x == 3 = o w * 1 + on (tail a)
--    | x == 1 = o w * on (tail a)
--    | otherwise = on (tail a)
-- 
-- o x
--    | x <= 3 = 1
--    | otherwise = 0

-- data Tree = Node [Tree] | Outlet | Leaf
-- 
-- tree :: [Int] -> Tree
-- tree [] = error "tree []"
-- tree [_] = Leaf
-- tree as = Node $ uncurry (++) $ (outs &&& map tree) $ takeWhile (compatible . head) $ init $ tails (tail as)
--    where
--    compatible = (>= head as - 3)
--    outs [] = []
--    outs (x:xs)
--       | 3 >= head x = Outlet : outs xs
--       | otherwise = outs xs
-- 
-- solve' :: [Int] -> Integer
-- solve' = outlets . tree . uncurry (:) . ((+3) . head &&& id) . sortBy (flip compare)
-- 
-- outlets :: Tree -> Integer
-- outlets = go 0
--    where
--    go acc (Node ts) = foldl' go acc ts
--    go acc Outlet = acc + 1
--    go acc Leaf = acc
-- 
