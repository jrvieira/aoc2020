import Zero.Zero hiding (test)
import Data.List as L
import Data.List.Split
import Control.Arrow
import Data.Map.Strict as M

test = do
   input <- fmap food . lines <$> readFile "21.test"
-- print $ match input
   print $ L.filter (flip notElem (match input)) $ ins input
   teqt "part 1" 5 $ length $ L.filter (flip notElem (match input)) $ ins input
   teqt "part 2" "mxmxvkd,sqjhc,fvjkl" $ intercalate "," $ elems $ match input

main = do
   input <- fmap food . lines <$> readFile "21.txt"
-- print $ match input
   print $ length $ L.filter (flip notElem (match input)) $ ins input
   -- part 2
   print $ intercalate "," $ elems $ match input

data Food = Food { ingredients :: [String] , allergens :: [String] }
   deriving Show

food :: String -> Food
food s = Food is as
   where
   (is,as) = (splitOn " " . head &&& splitOn ", " . last) $ init <$> splitOn "(contains " s

match :: [Food] -> Map String String
match = go empty
   where
   -- add associated ingredients to each allergen
   -- intersect each list to find possible candidates for each allergen
   -- finally deduce by process of elimination
   go :: Map String [[String]] -> [Food] -> Map String String
   go m [] = deduce $ L.foldr1 intersect <$> m
   go m (Food is as:fs) = go m' fs
      where
      m' = L.foldr ($) m ((\a -> M.insertWith (<>) a [is]) <$> as)

deduce :: Map String [String] -> Map String String
deduce = go empty
   where
   go :: Map String String -> Map String [String] -> Map String String
   go r m
   -- | False  # unlines [show r,show m] = undefined
      | empty == m = r
      | otherwise = go (r <> r') m'
      where
      r' :: Map String String
      r' = head <$> M.filter ((== 1) . length) m
      m' :: Map String [String]
      m' = L.filter (flip notElem r') <$> M.filter ((>= 2) . length) m

-- part 1

ins :: [Food] -> [String]
ins = L.foldr ((<>) . ingredients) []

