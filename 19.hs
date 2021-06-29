import Zero.Zero hiding (test)
import Data.List.Split
import qualified Data.IntMap.Strict as M
import Control.Applicative

test :: IO ()
test = do
    [i1,i2] <- splitOn "\n\n\n" <$> readFile "19.test"
    let input = parse i1
    let input2a = parse i2
    let input2b = parse2 i2
    teqt "part1" 2 $ solve input
    print $ valids $ fst input
    print $ snd input
    let r = flip elem (valids $ fst input) <$> snd input
    let v = map (valid (fst input)) (snd input)
    teqt "match" r v
    teqt "part2a" 3 $ solve input2a
    print $ filter (valid $ fst input2a) (snd input2a)
    teqt "part2b" 12 $ solve input2b
    print $ filter (valid $ fst input2b) (snd input2b)

main :: IO ()
main = do
   input <- parse <$> readFile "19.txt"
   input2 <- parse2 <$> readFile "19.txt"
   print $ solve input
   print $ solve input2

data Rules = Node [Rules] | Option Rules Rules | Match Char
   deriving Show

parse :: String -> (Rules,[String])
parse s = (rules r,ss)
   where
   [r,ss] = splitOn [""] $ lines s

rules :: [String] -> Rules
rules ls = go 0
   where
   m = M.fromList $ index <$> ls
   index l = let [i,r] = splitOn ": " l in (read i,r)
   go i
      | ('"':c:'"':_) <- n , True = Match c
      | '|' ∈ n = Option (Node $ go <$> parseNums l) (Node $ go <$> parseNums r)
      | otherwise = Node $ go <$> parseNums n
      where
      n = m M.! i
      [l,r] = splitOn "|" n 

valids :: Rules -> [String]
valids t = go t [""]
   where
   go (Node rs) acc = foldr go acc rs
   go (Option l r) acc = go l acc <> go r acc
   go (Match c) acc = map (c:) acc

valid :: Rules -> String -> Bool
valid rs s = go rs (Just s) ∋ Just ""
   where
   go :: Rules -> Maybe String -> [Maybe String]
   go _ Nothing = [Nothing]
   go (Node []) s = [s]
   go _ (Just []) = [Nothing]
   go (Node (r:rs)) s = concatMap (go $ Node rs) $ go r s
   go (Option l r) s = go l s <> go r s
   go (Match c) s
      | (head <$> s) == Just c = [tail <$> s]
      | otherwise = [Nothing]

solve :: (Rules,[String]) -> Int
-- solve (r,ss) = length $ filter (∈ valids r) ss
solve (r,ss) = length $ filter (valid r) ss

-- part 2

parse2 :: String -> (Rules,[String])
parse2 s = (rules2 r,ss)
   where
   [r,ss] = splitOn [""] $ lines s

rules2 :: [String] -> Rules
rules2 ls = go 0
   where
   part2 = M.insert 8 "42 | 42 8" . M.insert 11 "42 31 | 42 11 31"
   m = part2 $ M.fromList $ index <$> ls
   index l = let [i,r] = splitOn ": " l in (read i,r)
   go i
      | ('"':c:'"':_) <- n , True = Match c
      | '|' ∈ n = Option (Node $ go <$> parseNums l) (Node $ go <$> parseNums r)
      | otherwise = Node $ go <$> parseNums n
      where
      n = m M.! i
      [l,r] = splitOn "|" n 

