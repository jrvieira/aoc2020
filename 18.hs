import Zero.Zero
import Data.Char
import Control.Monad

main :: IO ()
main = do
   input <- lines <$> readFile "18.txt"
   tests <- lines <$> readFile "18.test"
-- teqt "part 1" [71,51,26,437,12240,13632] $ eval <$> tests
   print $ eval $ head tests

-- solve :: [String] -> Integer
-- solve = sum . map eval

-- eval :: String -> Integer
eval = symbols >=> balanced -- symbols >=> parens >=> tokens >=> lexemes >=> syntax

type Π a b = a -> Either String b

data Symbol = Num Integer | Add | Mul | Opn | Cls
   deriving Show

symbols :: Π [Char] [Symbol]
symbols [] = Right []
symbols (x:xs)
   | isDigit x = ((Num $ read $ takeWhile isDigit (x:xs)) :) <$> symbols (dropWhile isDigit xs)
   | '+' <- x = (Add :) <$> symbols xs
   | '*' <- x = (Mul :) <$> symbols xs
   | '(' <- x = (Opn :) <$> symbols xs
   | ')' <- x = (Cls :) <$> symbols xs
   | ' ' <- x = symbols xs
   | otherwise = Left $ "invalid symbol: " ++ show x

data Parens = Parens !Int !Int
   deriving Eq

instance Semigroup Parens where
   (Parens a b) <> (Parens c d)
      | b <= c = Parens a (d + b - c)
      | otherwise = Parens (a + c - b) d

instance Monoid Parens where
   mempty = Parens 0 0

parens Opn = Parens 1 0
parens Cls = Parens 0 1
parens _ = Parens 0 0

balanced :: Π [Symbol] [Symbol]
balanced xs 
   | foldMap parens xs == Parens 0 0 = Right xs
   | otherwise = Left "unmatched parens"

data Expr = Pro Expr Expr | Sum Expr Expr | Val Integer 

eval (Val n) = n
eval (Pro a b) = eval a + eval b
eval (Sum a b) = eval a * eval b

arythmetic :: Π [Symbol] Lexeme
arythmetic = go
   where
   go t xs
      | Opn <- head xs = takeWhile (/= Cls)
      | (l,r) <- break isOp = 
      where
      isOp x = x == Mul || x == Add

data Lexic = Node Token []

lexic :: Π Token Lexic
