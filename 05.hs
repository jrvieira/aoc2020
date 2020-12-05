{-# LANGUAGE BinaryLiterals #-}

import Zero.Zero
import Data.Bifunctor
import Control.Monad
import Data.Foldable

main :: IO ()
main = do
   teqt "test 1" 567 $ seat $ pass "BFFFBBFRRR"
   teqt "test 2" 119 $ seat $ pass "FFFBBBFRRR"
   teqt "test 3" 820 $ seat $ pass "BBFFBBFRLL"
   input <- lines <$> readFile "05.txt"
   print $ maximum $ (seat . pass) <$> input

type Pass = (Int,Int)

seat :: Pass -> Int
seat (r,c) = 8 * r + c

pass :: String -> Pass
pass = join bimap bin . break (∈ "RL")

bin :: String -> Int
bin = foldl' go 0
   where
   go a d = 2 * a + b d
   b d
      | d ∈ "BR" = 1
      | d ∈ "FL" = 0
      | otherwise = error "no bin parse"

-- part 2
