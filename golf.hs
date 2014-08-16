-- palindrome

palin a = a == reverse a

-- count vowels
count_vowels w = sum [ 1 | x <- w, x `elem` "AEIOUaeiou"]
