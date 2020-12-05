module Zero.Zero where

import Zero.Color
import Zero.Draw
import Debug.Trace

-- trace
infix 1 #
(#) :: a -> String -> a
(#) = flip trace

-- count elements
count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

-- last n elements
takeLast :: Int -> [a] -> [a]
takeLast n l = go (drop n l) l
   where
   go [] rs = rs
   go xs ys = go (tail xs) (tail ys)

-- split list on any elements
splitOnAny :: Eq a => [a] -> [a] -> [[a]]
splitOnAny d = go [] . reverse
   where
   go acc [] = acc : []
   go acc (c:cs)
      | c ∈ d = acc : go [] cs
      | otherwise = go (c : acc) cs

-- delete element from list
delete :: Eq a => a -> [a] -> [a]
delete _ [] = []
delete a (x:xs)
   | x == a = delete a xs
   | otherwise = x : delete a xs

-- remove element from assoc by key 
remove :: Eq k => k -> [(k,v)] -> [(k,v)]
remove _ [] = []
remove k (x:xs)
   | k == fst x = remove k xs
   | otherwise = x : remove k xs

-- remove element from assoc by value 
erase :: Eq v => v -> [(k,v)] -> [(k,v)]
erase _ [] = []
erase v (x:xs)
   | v == snd x = erase v xs
   | otherwise = x : erase v xs

-- tortoise and hare cycle finding
floyd :: Eq a => [a] -> Maybe (Integer,a)
floyd [] = Nothing
floyd (a:as) = go 1 1 a as
 where
  go _ _ _ [] = Nothing
  go pow lam x (y:ys)
   | x == y     = Just (lam,x)
   | pow == lam = go (2*pow) 1 y ys
   | otherwise  = go pow (1+lam) x ys

-- simple plot from list of ints
plot :: String -> [Int] -> IO ()
plot f = draw f . go 0 
   where
   go _ [] = []
   go n (x:xs) = [((n,y),True) | y <- [0..x]] ++ go (succ n) xs

-- test
test :: Show a => String -> (a -> Bool) -> a -> IO ()
test t p a = putStrLn $ unwords [t , clr c m , '\n' : clr c (show a) , "\n"]
   where
   (c,m)
      | p a = (Green,"v")
      | otherwise = (Red,"x")

teqt :: (Eq a,Show a) => String -> a -> a -> IO ()
teqt t e a = putStrLn $ unwords [t , clr c m , r , '\n' : clr c (show a) , "\n"]
   where
   (c,m,r)
      | a == e = (Green,"v","")
      | otherwise = (Red,"x",'\n' : show e)

-- elem
(∈) :: (Foldable t, Eq a) => a -> t a -> Bool
(∈) = elem

(∉) :: (Foldable t, Eq a) => a -> t a -> Bool
(∉) e = not . elem e

(∋) :: (Foldable t, Eq a) => t a -> a -> Bool
(∋) = flip elem
