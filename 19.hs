import Zero.Zero hiding (test)
import Data.List.Split
import qualified Data.IntMap.Strict as M
import Control.Applicative

-- test :: IO ()
-- test = do
--    input <- parse <$> readFile "19.test"
--    teqt "test" 2 $ solve input
--    print $ valids $ fst input
--    print $ snd input
--    let r = flip elem (valids $ fst input) <$> snd input
--    let v = map (validate (fst input)) (snd input)
--    teqt "match" r v

main :: IO ()
main = do
   input <- parse <$> readFile "19.txt"
   print $ solve input

parse :: String -> (Rules,[String])
parse s = (rules r,ss)
   where
   [r,ss] = splitOn [""] $ lines s

data Rules = Node [Rules] | Option Rules Rules | Match Char
   deriving Show

rules :: [String] -> Rules
rules ls = go 0
   where
   m = M.fromList $ zip [0..] $ drop 3 <$> ls
   go i
      | ('"':c:'"':_) <- n , True = Match c
      | '|' ∈ n = Option (Node $ go <$> parseNums l) (Node $ go <$> parseNums r)
      | otherwise = Node $ go <$> parseNums n
      where
      n = m M.! i
      [l,r] = splitOn ['|'] n 

-- valids :: Rules -> [String]
-- valids t = go t [""]
--    where
--    go (Node rs) acc = foldr go acc rs
--    go (Option l r) acc = go l acc <> go r acc
--    go (Match c) acc = map (c:) acc

validate :: Rules -> String -> Bool
validate rs s = go rs (Just s) == Just ""
   where
   go :: Rules -> Maybe String -> Maybe String
   go _ Nothing = Nothing
   go (Node []) s = s
   go _ (Just []) = Nothing
   go (Node (r:rs)) s = go (Node rs) (go r s)
   go (Option l r) s = go l s <|> go r s
   go (Match c) s
      | (head <$> s) == Just c = tail <$> s
      | otherwise = Nothing

solve :: (Rules,[String]) -> Int
-- solve (r,ss) = length $ filter (∈ valids r) ss
solve (r,ss) = length $ filter (validate r) ss

