{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit x = snd (x `divMod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit x = fst (x `divMod` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x = if x <= 0 then [] else lastDigit x : toRevDigits (dropLastDigit x) 
        

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | length xs > 0 = head xs : head (tail xs) * 2:[] ++ doubleEveryOther (tail(tail xs))
    | otherwise = []


-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (\x -> fst (x `divMod` 10) + snd (x `divMod` 10)) xs)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = snd ((sumDigits (doubleEveryOther (toRevDigits x))) `divMod` 10) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n == 1 = [(a, b)]
    | otherwise = hanoi (n-1) a c b ++ hanoi 1 a b c ++ hanoi (n-1) c b a



