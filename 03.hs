import Zero.Zero

main :: IO ()
main = do
   input <- readFile "03.txt"
   print $ slope (3,1) input
   print . product $ map (($ input) . slope) [(1,1),(3,1),(5,1),(7,1),(1,2)]

slope :: (Int,Int) -> String -> Int
slope (r,d) = count '#' . takes . skips . lines
   where
   skips [] = []
   skips xs = head xs : skips (drop d xs)
   takes xs = head <$> zipWith drop [0,r..] (cycle <$> xs)

