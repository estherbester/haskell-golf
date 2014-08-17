import Data.Char

-- palindrome

palin a = a == reverse a

-- count vowels
count_vowels w = sum [ 1 | x <- w, x `elem` "AEIOUaeiou"]

-- parity
-- returns true iff there are an odd number of 1s in the list
parity w = if odd (sum[1 | x <- w, x=='1']) then True else False

-- alt_caps
r = cycle [toUpper, toLower]
alt_caps w = [(fst x) (snd x) | x <- zip r w] 
