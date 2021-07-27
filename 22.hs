import Zero.Zero hiding (test)
import Zero.Queue
import Data.Function
import Data.Foldable as F
import Data.List
import Data.List.Split
import Data.IntMap.Strict as M hiding (null,map,foldr)
import Control.Arrow

test :: IO ()
test = do
   input <- parse <$> readFile "22.test"
   let part1 = combat input
   teqt "part 1" (2,queue [3,2,10,6,8,5,9,4,7,1]) part1
   teqt "part 1" (2,306) $ score <$> part1
   let part2 = reccom input
   teqt "part 2" (2,queue [7,5,6,2,4,1,10,8,9,3]) part2
   teqt "part 2" (2,291) $ score <$> part2

main :: IO ()
main = do
   input <- parse <$> readFile "22.txt"
   print $ score $ snd $ combat input
   print $ score $ snd $ reccom input

parse :: String -> IntMap Deck
parse = fromList . map (head . parseNums . head &&& queue . map read . tail) . splitOn [""] . lines

type Deck = Queue Int

combat :: IntMap Deck -> (Int , Deck)
combat ds
-- | False  # show hands = undefined
   | size ds' == 1 = (taker , ds ! taker)
   | otherwise = combat ds'
   where
   ds' = M.adjust (<> queue cards) taker hands
   taker = fst $ head round
   cards = card <$> round
   hands = snd <$> M.fromList round
   round = sortBy (flip $ on compare card) $ M.toList $ first (maybe undefined id) . dequeue <$> M.filter (not . null) ds
   card (_,(x,_)) = x

score :: Deck -> Int
score = sum . zipWith (*) [1..] . reverse . foldr (:) []

-- part 2 (recursive combat)

reccom :: IntMap Deck -> (Int , Deck)
reccom = go . pure
   where
   go :: [IntMap Deck] -> (Int , Deck)
   go (ds:hs)
   -- | False  # show dsφ = undefined
      | size dsφ == 1 = findMin dsφ
      | ds ∈ hs = (1 , ds ! 1)
      | and $ uncurry (<=) . second length <$> elems round = go $ M.adjust (<> queue (cards ! recwin : M.elems (M.delete recwin cards))) recwin hands : ds : hs
      | otherwise = go $ ds' : ds : hs
      where
      ds' = M.adjust (<> queue (fst . snd <$> combat)) (fst $ head combat) hands
      combat = sortBy (flip $ on compare (fst . snd)) $ M.toList round
      recwin = fst $ reccom $ queue . uncurry take . second F.toList <$> round
      cards = fst <$> round
      hands = snd <$> round
      round = first (maybe undefined id) . dequeue <$> M.filter (not . null) dsφ
      dsφ = M.filter (not . null) ds

