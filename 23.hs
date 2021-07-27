import Zero.Zero
import Data.Char
import Data.List
import Data.List.Split
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
cups n = map intToDigit . snip . (!! n) . iterate move . map digitToInt
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

cups' :: String -> Integer
cups' = product . map toInteger . take 2 . (!! 10000000) . iterate move . (<> [10..1000000]) . map digitToInt

