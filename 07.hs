{-# LANGUAGE DeriveFoldable #-}

import Zero.Zero
import Data.List.Split
import Data.Foldable
import qualified Data.Map.Lazy as M
import Control.Arrow

main :: IO ()
main = do
   input <- map bag . lines <$> readFile "07.txt"
   print $ subtract 1 $ length $ filter (Color "shiny gold" ∈) $ forest input
-- test <- map bag . lines <$> readFile "07.test"
-- teqt "part 2" (Just 126) $ fmap total $ find ((Color "shiny gold" ==) . ε) $ forest test2
   print $ fmap (subtract 1 . total) $ find ((Color "shiny gold" ==) . ε) $ forest input

newtype Color = Color String
   deriving (Eq,Ord)

type Contents = [(Int,Color)]
type Bag = (Color,Contents)

data Tree a = Node { ε :: a , ν :: Int , τ :: [Tree a] }
   deriving Foldable

forest :: [Bag] -> [Tree Color]
forest bags = tree 1 <$> bags
   where
   tree n (color,contents) = Node color n (uncurry tree . fmap (id &&& (β M.!)) <$> contents)
   β = M.fromList bags

-- parser

bag :: String -> Bag
bag s = (Color color,contents)
   where
   color:rest:_ = splitOn " bags contain " s
   contents = foldl' p [] $ splitOn ", " (init rest)
   p acc c
      | "no" <- n = acc
      | otherwise = (read n,Color $ unwords $ init b) : acc
      where
      (n:b) = words c

-- part 2

total :: Tree a -> Int
total (Node _ m []) = m
total (Node _ m ts) = m + m * sum (total <$> ts)

