import Zero.Zero
import Control.Arrow

main :: IO ()
main = do
   input <- map (head &&& read . tail) . lines <$> readFile "12.txt"
   test <- map (head &&& read . tail) . lines <$> readFile "12.test"
   teqt "part 1" 25 $ solve test
   print $ solve  input
   teqt "part 2" 286 $ solve' test
   print $ solve' input

data Position = Π { x :: Int , y :: Int }

data Ship = Ship { δ :: Char , π :: Position }

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
   | otherwise = run s { δ = t } xs
   where
   t = head . drop (div n 90) . dropWhile (/= d) $ cycle wise
   wise
      | 'L' <- di = "NWSE"
      | 'R' <- di = "WNES"
      | otherwise = error "no parse"

-- part 2

data Ship' = Ship' { σ :: Position , ω :: Position }

solve' :: [(Char,Int)] -> Int
solve' = uncurry (+) . (abs . x &&& abs . y) . σ . run' (Ship' (Π 0 0) (Π 10 1))

run' :: Ship' -> [(Char,Int)] -> Ship'
run' s [] = s
run' s@(Ship' p w) ((di,n):xs)
-- | False  # show ((di,n),s) = undefined
   | 'N' <- di = run' s { ω = Π (x w) (y w + n) } xs
   | 'S' <- di = run' s { ω = Π (x w) (y w - n) } xs
   | 'E' <- di = run' s { ω = Π (x w + n) (y w) } xs
   | 'W' <- di = run' s { ω = Π (x w - n) (y w) } xs
   | 'F' <- di = run' s { σ = Π (x p + x w * n) (y p + y w * n) } xs
   | 0 <- n = run' s xs
   | 'L' <- di = run' s { ω = Π (-y w) (x w) } $ (di,n-90) : xs
   | 'R' <- di = run' s { ω = Π (y w) (-x w) } $ (di,n-90) : xs
   | otherwise = error "no parse"
