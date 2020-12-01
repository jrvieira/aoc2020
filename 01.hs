import qualified Data.Set as S

main :: IO ()
main = do
   list <- map read . lines <$> readFile "01.txt"
   print $ solve 2020 list
   print $ solve' 2020 list

solve :: Int -> [Int] -> Either String Int
solve _ [] = Left "empty set"
solve t ls = go ls
   where
   set = S.fromList ls
   go [] = Left "no match"
   go (x:xs)
      | S.member (t - x) set = Right $ x * (t - x)
      | otherwise = go xs

-- part 2

solve' :: Int -> [Int] -> Either String Int
solve' _ [] = Left "empty set"
solve' t ls = go ls
   where
   go (x:xs)
      | Right n <- solve (t - x) ls = Right (x * n)
      | otherwise = go xs
