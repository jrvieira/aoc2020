import Zero.Zero
import Data.List.Split
import Control.Arrow
import Text.Read (readMaybe)

main :: IO ()
main = do
   input <- map (splitOnAny " \n") . splitOn "\n\n" . init <$> readFile "04.txt"
   print $ count id $ validate  . passport <$> input
   print $ count id $ validate' . passport <$> input

type Passport = [(String,String)]

fields :: [String]
fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

validate :: Passport -> Bool
validate p = and $ ($ fs) <$> (elem <$> fields)
   where
   fs = fst <$> p

-- part 2

passport :: [String] -> Passport
passport = map (fmap tail . break (== ':'))

validate' :: Passport -> Bool
validate' = uncurry (&&) . (validate &&& all valid)
   where
   valid (f,v)
      | "byr" <- f , Just n <- readMaybe v :: Maybe Int = n >= 1920 && n <= 2002
      | "iyr" <- f , Just n <- readMaybe v :: Maybe Int = n >= 2010 && n <= 2020
      | "eyr" <- f , Just n <- readMaybe v :: Maybe Int = n >= 2020 && n <= 2030
      | "hgt" <- f , (v,"cm") <- span (∈ ['0'..'9']) v , Just n <- readMaybe v :: Maybe Int = n >= 150 && n <= 193
      | "hgt" <- f , (v,"in") <- span (∈ ['0'..'9']) v , Just n <- readMaybe v :: Maybe Int = n >= 59 && n <= 76
      | "hcl" <- f , '#':hx <- v , Just n <- readMaybe ("0x" ++ hx) :: Maybe Int = length hx == 6
      | "ecl" <- f = v ∈ ["amb","blu","brn","gry","grn","hzl","oth"]
      | "pid" <- f , Just n <- readMaybe v :: Maybe Int = length v == 9
      | "cid" <- f = True
      | otherwise = False

