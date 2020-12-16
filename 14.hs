import Zero.Zero
import Data.Bits
import Data.Foldable
import Data.List.Split
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Control.Arrow

main :: IO ()
main = do
   input <- map parse . lines <$> readFile "14.txt"
   tests <- map (map parse . lines) . splitOn "\n\n" <$> readFile "14.test"
   teqt "part 1" 165 $ solve (head tests)
   print $ solve  input
   teqt "part 2" 208 $ solve' (last tests)
   print $ solve' input

type Mem = Map Integer Integer
type Mask = [Maybe Bool]
data Instruction = Write Integer Integer | Mask Mask
   deriving Show

parse :: String -> Instruction
parse s
   | "mask" <- take 4 s = Mask $ mask $ drop 7 s
   | "mem[" <- take 4 s = uncurry Write $ (fromIntegral . head &&& last) $ parseNums s
   | otherwise = error "no parse"

solve :: [Instruction] -> Integer
solve = sum . snd . foldl' run (mask "",M.empty)
   where
   run (msk,mem) i
      | Write p v <- i = (msk,M.insert p (μ msk v) mem)
      | Mask msk <- i = (msk,mem)

mask :: String -> Mask
mask s = b <$> s
   where
   b '1' = Just True
   b '0' = Just False
   b 'X' = Nothing
   b  _  = error "mask: no parse"

μ :: (Bits a,Num a) => Mask -> a -> a
μ m = go 0 (reverse m)
   where
   go _ [] = id
   go n (x:xs)
      | Nothing <- x = go (succ n) xs
      | Just True <- x = go (succ n) xs . flip setBit n
      | Just False <- x = go (succ n) xs . flip clearBit n

-- part 2

solve' :: [Instruction] -> Integer
solve' = sum . snd . foldl' run (mask "",M.empty)
   where
   run (msk,mem) i
      | Write p v <- i = (msk,inserts p v)
      | Mask msk <- i = (msk,mem)
      where
      inserts :: Integer -> Integer -> Mem
      inserts p v = foldr' (flip M.insert v) mem (μ' p)
      μ' :: Integer -> [Integer]
      μ' p = go [p] 0 (reverse msk)
         where
         go acc _ [] = acc
         go acc n (x:xs)
            | Just True <- x = go (flip setBit n <$> acc) (succ n) xs
            | Nothing <- x = go ([flip setBit n,flip clearBit n] <*> acc) (succ n) xs
            | otherwise = go acc (succ n) xs

