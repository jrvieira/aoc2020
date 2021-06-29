import Zero.Zero hiding (test)
import qualified Data.IntMap.Strict as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Ord
import Control.Arrow

test :: IO ()
test = do
   input <- parse <$> readFile "20.txt"
   print $ length input
-- print $ sortSize $ matches input
   print $ M.map length $ M.filter ((< 4) . length) $ matches input
   teqt "tsubmodule" "yes" "yes"
   teqt "fsubmodule" "yes" "no"

main :: IO ()
main = do
   print "main"

type Border = [Bool]

parse :: String -> M.IntMap [Border]
parse = M.fromList . map (head . parseNums . head &&& borders . unlines . tail) . splitOn [""] . lines

borders :: String -> [Border]
borders s = map (grid M.!) <$> ixes
   where
   grid = M.fromList $ zip [0..] $ (== '#') <$> s
   side = round $ sqrt $ fromIntegral $ size
   size = length s
   ixes = take side <$> [
      [0..],
      [0,side..],
      [pred side,pred side + side..],
      [pred size - side..]
      ]

-- preliminary
matches :: M.IntMap [Border] -> M.IntMap (S.Set Int)
matches m = M.mapWithKey (go S.empty) m
   where
   go :: S.Set Int -> Int -> [Border] -> S.Set Int
   go acc _ [] = acc
   go acc i (b:bs) = go (S.fromList (M.keys $ M.filter (match b) $ M.delete i m) <> acc) i bs
   match :: Border -> [Border] -> Bool
   match b bs = b ∈ bs || reverse b ∈ bs

sortSize  :: M.IntMap (S.Set Int) -> [(Int,Int)]
sortSize = sortBy (comparing snd) . M.toList . M.map S.size

