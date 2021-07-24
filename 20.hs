{-# LANGUAGE FlexibleInstances #-}

import Zero.Zero hiding (test)
import Data.Foldable
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Ord
import Control.Arrow
import Control.Monad

test :: IO ()
test = do
   input <- parse <$> readFile "20.test"
-- teqt "part1" 20899048083289 $ product $ map fromIntegral $ M.keys $ M.filter ((== 2) . S.size) $ matches input
-- print $ M.map S.size $ matches input
-- let t = toT $ (== '#') <$> "..##.#..#.##..#.....#...##..#.####.#...###.##.###.##...#.###.#.#.#..##..#....#..###...#.#...###..###"
-- mapM_ print$ post t <$> [ Pos r f | f <- total , r <- total ]
-- print $ post (τ t) (Pos R3 True)
-- print $ picture (grid $ M.elems input)
   print $ monsters $ picture (grid $ M.elems input)
   let p = picture (grid $ M.elems input)
   let Just monstern = monsters p
   let waters = length $ M.filter id p
   let roughness = waters - length monster * monstern
   teqt "part2" 2 monstern
   teqt "part2" 273 roughness

main :: IO ()
main = do
   input <- parse <$> readFile "20.txt"
   print $ product $ map fromIntegral $ M.keys $ M.filter ((== 2) . S.size) $ matches input
   let p = picture (grid $ M.elems input)
   let Just monstern = monsters p
   let waters = length $ M.filter id p
   let roughness = waters - length monster * monstern
   print roughness

type Tile = [Bool]
type Border = [Bool]

tile :: String -> Tile
tile = map (== '#')

parse :: String -> M.Map Int Tile
parse = M.fromList . map (head . parseNums . head &&& tile . join . tail) . splitOn [""] . lines

{- part1
   turns out there are only 4 possible corners (4 vertices with onlu 2 edges)
   so just find them and their product -}

matches :: M.Map Int Tile -> M.Map Int (S.Set Coords)
matches m = M.mapWithKey (go S.empty) m'
   where
   m' = M.map borders m
   go :: S.Set Coords -> Int -> [Border] -> S.Set Coords
   go acc _ [] = acc
   go acc i (b:bs) = go (S.fromList (M.keys $ grid $ toList $ M.filter (match b) $ M.delete i m') <> acc) i bs
   match :: Border -> [Border] -> Bool
   match b bs = b ∈ bs || reverse b ∈ bs
   borders :: Tile -> [Border]
   borders m = map (g M.!) <$> ixes
      where
      g = M.fromList $ zip [0..] m
      s = round $ sqrt $ fromIntegral size
      size = length m
      ixes = [
         [0..pred s],
         [pred s,pred s + s..pred size],
         reverse [size - s..pred size],
         reverse [0,s..size - s]
         ]

{- part2
   turns out there are exactly 4 possible corners, 10*4 possible edges and 10^2 possible inner tiles
   (with exactly 2, 3 and 4 edges respectively)

   sortSize  :: M.Map Coords (S.Set) -> [Coords]
   sortSize = sortBy (comparing snd) . M.toList . M.map S.size -}

type Grid = M.Map Coords

newtype Coords = C (Int,Int)
   deriving (Show,Eq)

instance Ord Coords where
   compare (C a) (C b) = (comparing snd <> comparing fst) a b

instance Semigroup Coords where
   C (xa,ya) <> C (xb,yb) = C (xa + xb,ya + yb)

data T = T { τ :: Grid Bool , β :: [Bool] }
   deriving Show

data Pos = Pos { ρ :: Rot , φ :: Flip }
   deriving Show

data Rot = R0 | R1 | R2 | R3 -- R * 90º ccw
   deriving (Show,Bounded,Enum)

type Flip = Bool

data Dir = N | E | S | W
   deriving (Bounded,Enum)

instance {-# OVERLAPPING #-} Show (Grid Bool) where
   show = unlines . map (intersperse ' ') . uncurry chunksOf . (side &&& map (\b -> if b then '#' else '.')) . M.elems

instance {-# OVERLAPPING #-} Show Tile where
   show = map (\x -> if x then '#' else '.')

coords s = C <$> [ (x,y) | y <- take s [0..] , x <- take s [0..] ]

grid :: [a] -> Grid a
grid = M.fromList . uncurry zip . (coords . side &&& id)

side :: Foldable f => f a -> Int
side = round . sqrt . fromIntegral . length

toT :: Tile -> T
toT t = T g b
   where
   g = grid $ (t !!) <$> inner
   b = (t !!) <$> borders
   inner = filter (not . (∈ borders)) $ take (length t) [0..]
   borders =
      take s [0..] <>
      take s [pred s, pred s + s..] <>
      reverse (take s [s ^ 2 - s..]) <>
      reverse (take s [0, s..])
   s = side t

-- grid transform
post :: Grid a -> Pos -> Grid a
post g (Pos r f) = rot r . flp f $ g

-- grid rotation
rot :: Rot -> Grid a -> Grid a
rot r g = g'
   where
   g' = grid $ (list !!) <$> rix
   list = M.elems g
   rix = (!! fromEnum r) . iterate r90 <$> take (s ^ 2) [0..]
   r90 n = let (x,y) = xy n in i (pred s - y,x)
   xy n = (rem n s,div n s)
   i (x,y) = y * s + x
   s = side g

-- grid flip
flp :: Bool -> Grid a -> Grid a
flp b g = if b then g' else g
   where
   g' = grid $ (list !!) <$> rix
   list = M.elems g
   rix = f <$> take (s ^ 2) [0..]
   f n = let (x,y) = xy n in i (y,x)
   xy n = (rem n s,div n s)
   i (x,y) = y * s + x
   s = side g

pborders :: (T,Pos) -> [Bool]
pborders (T g b,Pos r f) = take ((s + 2) * 4) $ drop ((s + 2) * fromEnum r) $ cycle b'
   where
   b'
     | f = reverse b
     | otherwise = b
   s = side g

adjs :: Coords -> [Coords]
adjs (C (x,y)) = C <$> [(x,pred y),(succ x,y),(x,succ y),(pred x,y)]

border :: Dir -> (T,Pos) -> [Bool]
border d (t,p) = take s $ drop (fromEnum d * s) $ pborders (t,p)
   where
   s = 2 + side (τ t)

-- assume matches are unique
picture :: M.Map Coords Tile -> Grid Bool
picture m = pic puzzle -- # unlines (show . snd <$> toList puzzle)
   where
   puzzle :: Grid (T,Pos)
   puzzle = f (M.singleton (C (0,0)) (head ts,Pos R0 False)) (S.fromList $ adjs (C (0,0))) (tail ts)
   ts :: [T]
   ts = map toT $ M.elems m

--   picture         borderIxs       deck
f :: Grid (T,Pos) -> S.Set Coords -> [T] -> Grid (T,Pos)
f g _ [] = g
f g b ts = go [] ts
   where
   bs = S.toList b
   --    tried  to_try
   go :: [T] -> [T] -> Grid (T,Pos)
   go mem [] = error $ "no match while " <> show (length g) <> ":" <> show (length mem)
   go mem (t:tt)
      | Nothing <- p = go (t:mem) tt
      | Just (i,Just pos) <- p , g' <- M.insert i (t,pos) g = f g' ((S.fromList (adjs i) <> b) S.\\ M.keysSet g') (mem <> tt)
      where
      p = find (isJust . snd) $ (id &&& fit g t) <$> bs

pic :: Grid (T, Pos) -> Grid Bool
pic g = g'
   where
   g' = grid blines
   blines :: [Bool]
   blines = concat $ concatMap (concat . transpose) plines
   plines :: [[[[Bool]]]]
   plines = uncurry chunksOf $ (side &&& id) $ uncurry chunksOf . (side &&& M.elems) . uncurry post . first τ <$> M.elems g

fit :: Grid (T,Pos) -> T -> Coords -> Maybe Pos
fit g t (C (x,y)) = find f poss -- # "t fitness: " <> unwords [show (f <$> poss),show (x,y)]
   where
   poss :: [Pos]
   poss = [ Pos r f | f <- total , r <- total ]
   f :: Pos -> Bool
   f p = all match $ zip (chunksOf (2 + side (τ t)) $ pborders (t,p)) badj -- # "b matches: " <> unwords [show $ zipWith (curry match) (chunksOf (2 + side (τ t)) $ pborders (t,p)) badj,show (x,y)]
   match :: ([Bool],Maybe [Bool]) -> Bool
   match (_,Nothing) = True
   match (a,Just b) = a == b -- # unlines [show a,"  " <> show b]
   badj :: [Maybe [Bool]]
   badj = [
      reverse . border S <$> adj N,
      reverse . border W <$> adj E,
      reverse . border N <$> adj S,
      reverse . border E <$> adj W
      ]
   adj :: Dir -> Maybe (T,Pos)
   adj d = g M.!? (adjs (C (x,y)) !! fromEnum d)

monster :: [Coords]
monster = M.keys $ M.filter id $ M.fromList $ zip [ C (x,y) | y <- [0..2] , x <- [0..19] ] $ (== '#') <$> m
   where
   m = "                  # " <>
       "#    ##    ##    ###" <>
       " #  #  #  #  #  #   "

-- scan the image for monsters
monsters :: Grid Bool -> Maybe Int
monsters g = find (> 0) $ found <$> poss -- # unlines (show <$> poss) <> show (found <$> poss)
   where
   poss :: [Grid Bool]
   poss = post g <$> [ Pos r f | f <- total , r <- total ]
   found :: Grid Bool -> Int
   found p = foldl' go 0 $ M.keys p
      where
      go :: Int -> Coords -> Int
      go n c
--       | False  # show (map (maybe False id . (p M.!?)) $ (c <>) <$> monster, show c) = undefined
         | C (x,y) <- c , side g - 20 < x || side g - 3 < y = n -- ignore out of boundout of boundss
         | all (maybe False id . (p M.!?)) $ (c <>) <$> monster = succ n
         | otherwise = n

