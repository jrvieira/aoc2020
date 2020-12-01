import qualified Data.Set as S

main :: IO ()
main = do
   list <- map read . lines <$> readFile "01.txt"
   print $ solve 2020 list
   print $ solve' 2020 list

solve :: Int -> [Int] -> Maybe Int
solve t ls = go ls
   where
   set = S.fromList ls
   go [] = Nothing
   go (x:xs)
      | S.member (t - x) set = Just (x * t - x)
      | otherwise = go xs

-- part 2

solve' :: Int -> [Int] -> Maybe Int
solve' t ls = go ls
   where
   go [] = Nothing
   go (x:xs)
      | Just n <- solve (t - x) ls = Just (x * n)
      | otherwise = go xs

