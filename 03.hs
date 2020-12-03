import Zero.Zero

main :: IO ()
main = do
   input <- readFile "03.txt"
   print . product $ map (($ input) . slope) [(1,1),(3,1),(5,1),(7,1),(1,2)]

slope :: (Int,Int) -> String -> Int
slope (r,d) = count '#' . tk 0 . every d . lines
   where
   tk _ [] = []
   tk n (x:xs) = cycle x !! n : tk (n+r) xs

every :: Int -> [a] -> [a]
every _ [] = []
every n (x:xs) = x : every n (drop (n-1) xs)
