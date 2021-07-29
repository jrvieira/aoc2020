import Zero.Zero
import Data.Char
import Data.List
import qualified Data.Set as S

test :: IO ()
test = do
   input <- parse <$> readFile "24.test"
   teqt "part 1" 10   $ length $ solve  input
   teqt "part 2" 2208 $ length $ solve' input

main :: IO ()
main = do
   input <- parse <$> readFile "24.txt"
   print $ length $ solve  input
   print $ length $ solve' input

{- TILES

 nw o o ne     / .
 w o * o e  --*--y
 sw o o se   /   .
           .x....+
-}

type Pos = (Int,Int)
type Floor = S.Set Pos

data Dir = NE | E | SE | SW | W | NW
   deriving (Read,Bounded,Enum,Ord,Eq)

parse :: String -> [[Dir]]
parse = fmap δ . lines
   where
   δ :: String -> [Dir]
   δ "" = []
   δ ds = read (toUpper <$> dir) : δ rest'
      where
      (dir,rest')
         | head d ∈ "we" = (<> rest) <$> splitAt 1 d
         | otherwise = (d,rest)
      (d,rest) = splitAt 2 ds

adj :: Pos -> Dir -> Pos
adj (x,y) dir
   | NE <- dir = (     x,pred y)
   |  E <- dir = (succ x,     y)
   | SE <- dir = (succ x,succ y)
   | SW <- dir = (     x,succ y)
   |  W <- dir = (pred x,     y)
   | NW <- dir = (pred x,pred y)

dest :: [Dir] -> Pos
dest = foldl' adj (0,0)

solve :: [[Dir]] -> Floor
solve = foldl' turn S.empty . fmap dest

turn :: Floor -> Pos -> Floor
turn f p = op p f
   where
   op
      | S.member p f = S.delete
      | otherwise = S.insert

-- part 2

solve' :: [[Dir]] -> Floor
solve' = (!! 100) . iterate sim . solve
   where
   sim :: Floor -> Floor
   sim f = survive <> born -- # unwords (show <$> [length $ survive <> born])
      where
      survive = S.filter ((∈ [1,2]) . adjs) f
      born    = S.filter ((∈ [2]  ) . adjs) α
      α = S.fromList $ concatMap (\p -> adj p <$> total) $ S.toList f
      adjs :: Pos -> Int
      adjs p = length $ S.intersection (S.map (adj p) τ) f

τ :: S.Set Dir
τ = S.fromList total

