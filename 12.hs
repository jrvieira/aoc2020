import Zero.Zero
import Control.Arrow

main :: IO ()
main = do
   input <- map (head &&& read . tail) . lines <$> readFile "12.txt"
   test <- map (head &&& read . tail) . lines <$> readFile "12.test"
   teqt "part 1" 25 $ solve test
   print $ solve  input

data Position = Π { x :: Int , y :: Int }
   deriving Show

data Ship = Ship { δ :: Char , π :: Position }
   deriving Show

solve :: [(Char,Int)] -> Int
solve = uncurry (+) . (abs . x &&& abs . y) . π . run (Ship 'E' (Π 0 0))

run :: Ship -> [(Char,Int)] -> Ship
run s [] = s
run s@(Ship d p) ((di,n):xs)
-- | False  # show ((di,n),s) = undefined
   | 'N' <- di = run s { π = Π (x p) (y p + n) } xs
   | 'S' <- di = run s { π = Π (x p) (y p - n) } xs
   | 'E' <- di = run s { π = Π (x p + n) (y p) } xs
   | 'W' <- di = run s { π = Π (x p - n) (y p) } xs
   | 'F' <- di = run s $ (d,n) : xs
   | otherwise = run s { δ = t (div n 90) } xs
   where
   t x = head . drop x . dropWhile (/= d) $ cycle wise
   wise
      | 'L' <- di = "NWSE"
      | 'R' <- di = "WNES"
      | otherwise = error "no parse"

