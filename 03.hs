import Zero.Zero

main :: IO ()
main = do
   input <- readFile "03.txt"
   print $ slope (3,1) input
   print $ product $ ($ input) . slope <$> [(1,1),(3,1),(5,1),(7,1),(1,2)]

slope :: (Int,Int) -> String -> Int
slope (r,d) = count '#' . drops . skips . lines
   where
   skips [] = []
   skips xs = head xs : skips (drop d xs)
   drops xs = head <$> zipWith drop [0,r..] (cycle <$> xs)

