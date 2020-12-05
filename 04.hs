import Zero.Zero
import Util
import Data.List.Split
import Control.Arrow
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
   input <- map (splitOnAny " \n") . splitOn "\n\n" . init <$> readFile "04.txt"
   print $ count id $ validate  . passport <$> input
   print $ count id $ validate' . passport <$> input

type Passport = [(String,String)]

fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

validate :: Passport -> Bool
validate p = and $ ($ fs) <$> (elem <$> fields)
   where
   fs = fst <$> p

-- part 2

passport :: [String] -> Passport
passport = map (take 3 &&& drop 4)

validate' :: Passport -> Bool
validate' = uncurry (&&) . (all valid &&& validate)
   where
   valid ("byr",v) = uncurry (&&) $ (>= 1920) &&& (<= 2002) $ fromMaybe 0 $ readMaybe v
   valid ("iyr",v) = uncurry (&&) $ (>= 2010) &&& (<= 2020) $ fromMaybe 0 $ readMaybe v
   valid ("eyr",v) = uncurry (&&) $ (>= 2020) &&& (<= 2030) $ fromMaybe 0 $ readMaybe v
   valid ("hgt",v) = check $ fromMaybe 0 $ readMaybe $ takeWhile (`elem` ['0'..'9']) v
      where
      unit = dropWhile (`elem` ['0'..'9']) v
      check h
         | "cm" <- unit = uncurry (&&) $ ((>= 150) &&& (<= 193)) h
         | "in" <- unit = uncurry (&&) $ ((>= 59) &&& (<= 76)) h
         | otherwise = False
   valid ("hcl",'#':hex) = uncurry (&&) $ ((== 6) . length &&& all (`elem` (['0'..'9'] ++ ['a'..'f']))) hex
   valid ("ecl",v) = elem v ["amb","blu","brn","gry","grn","hzl","oth"]
   valid ("pid",v) = uncurry (&&) $ ((== 9) . length &&& all (`elem` ['0'..'9'])) v
   valid ("cid",_) = True
   valid _ = False

