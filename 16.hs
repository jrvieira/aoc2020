import Zero.Zero
import Data.List
import Data.List.Split
import Data.Bifunctor
import Control.Arrow

data Rule = Rule { field :: Int , ranges :: ((Int,Int),(Int,Int)) }
type Ticket = [Int]
type Input = ([Rule],[Ticket])

parse :: String -> Input
parse = bimap parseR parseT . (head &&& init . unlines . tail) . splitOn "\n\n" 

parseR :: String -> [Rule]
parseR = go 0 [] . lines
   where
   go _ acc [] = acc
   go n acc (x:xs)
      | [_,rs] <- splitOn ": " x , [mn0,mx0,mn1,mx1] <- parseNums rs = go (succ n) (Rule i ((mn0,mx0),(mn1,mx1)) : acc) xs
      | otherwise = error "no parseR"
      where
      i 
         | take 9 x == "departure" = n
         | otherwise = n + 20

parseT :: String -> [Ticket]
parseT = go [] . lines
   where
   go acc [] = acc
   go acc (x:xs)
      | x ∋ ':' = go acc xs
      | otherwise = go (parseNums x : acc) xs

main :: IO ()
main = do
   input <- parse <$> readFile "16.txt"
   test <- parse <$> readFile "16.test"
   teqt "part 1" 71 $ solve test
   print $ solve  input
   print $ solve' input

solve :: Input -> Int
solve (rules,tickets) = sum $ invalid <$> tickets
   where
   invalid = sum . filter (not . check)
   check x = go rules
      where
      go [] = False
      go (Rule _ ((mn0,mx0),(mn1,mx1)):rs) = x >= mn0 && x <= mx0 || x >= mn1 && x <= mx1 || go rs

-- part 2

solve' :: Input -> Integer
solve' (rules,tickets) = product $ fromIntegral . (last tickets !!) . fst <$> filter ((< 20) . snd) deduced
   where
   deduced = go [] $ sortOn (length . snd) $ zip [0..] constraint
      where
      go _ [] = []
      go acc ((i,ps):xs)
         | [s] <- ps \\ acc = (i,s) : go (ps ++ acc) xs
         | otherwise = error "indeductible"
   constraint = map (unique . foldr1 intersect) $ transpose $ valid $ map check <$> tickets
   valid = filter (all $ not . null)
   check :: Int -> [Int]
   check x = go rules
      where
      go [] = []
      go (Rule i ((mn0,mx0),(mn1,mx1)):rs)
         | x >= mn0 && x <= mx0 || x >= mn1 && x <= mx1 = i : go rs
         | otherwise = go rs

