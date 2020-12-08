import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Data.Maybe
import Data.Either

main :: IO ()
main = do
   input <- lines <$> readFile "08.txt"
   let mem = ini $ tape input
   print $ run mem
   print $ filter isRight $ map (run . ini) $ alts $ tape input 

type Pointer = Integer
type Accumulator = Integer
type Tape = Map Integer Op
data Op = HALT | ACC Integer | JMP Integer | NOP Integer | ERR String 
data Mem = Mem { τ :: Tape , π :: Pointer , α :: Accumulator }

ini :: Tape -> Mem
ini t = Mem t 0 0

tape :: [String] -> Tape
tape = M.fromList . zip [0..] . map op

op :: String -> Op
op s
   | ["acc",i] <- words s = ACC (int i)
   | ["jmp",i] <- words s = JMP (int i)
   | ["nop",i] <- words s = NOP (int i)
   | otherwise = error ("invalid op: " ++ s)
   where
   int ('+':x) = read x
   int ('-':x) = - read x
   int x = error ("no parse: " ++ x)

run :: Mem -> Either String Accumulator
run μ
   | HALT <- ο = Right $ α μ
   | ACC i <- ο = run  μ { τ = τ' , π = π μ + 1 , α = α μ + i }
   | JMP i <- ο = run  μ { τ = τ' , π = π μ + i }
   | NOP _ <- ο = run  μ { τ = τ' , π = π μ + 1 }
   | ERR e <- ο = Left e
   | otherwise = error "runtime error"
   where
   ο = fromMaybe (ERR $ "loop @p" ++ show (π μ) ++ " " ++ show (α μ)) $ τ μ M.!? π μ
   τ' = M.delete (π μ) (τ μ)

-- part 2

-- non-deterministic unique substitution:
-- get [ix] of all jmp/nop
-- map (change tape) [ix]
alts :: Tape -> [Tape]
alts t = change <$> (M.keys $ M.filter alt t)
   where
   change k = M.adjust adj k $ M.insert (fst $ M.findMax t) HALT t
   adj (JMP i) = NOP i
   adj (NOP i) = JMP i
   adj _ = error "alts error"
   alt (JMP _) = True
   alt (NOP _) = True
   alt _ = False

