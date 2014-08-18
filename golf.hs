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

-- is_triangle
-- i can't do this right now
is_triangle a b c
	| a + b <= c = False
	| a + c <= b = False 
	| b + c <= a = False
	| otherwise = True

-- next color
c = cycle ["GREEN", "YELLOW", "RED"]
next_color w = dropWhile (/= w) c !! 1 

-- fib
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2) 
    

