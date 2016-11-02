{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit i = i `mod` 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit i = i `div` 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits x
    | x <= 0 = []
    | otherwise = (lastDigit x) : toRevDigits (dropLastDigit x)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ys) = x:(2 * y):doubleEveryOther ys
doubleEveryOther (x:[]) = [x]
doubleEveryOther [] = []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.

sumOfDigits :: Integer -> Integer
sumOfDigits x = sum (toRevDigits x)

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map sumOfDigits xs)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn x = (sumDigits . doubleEveryOther . toRevDigits  $ x) `mod` 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
    | n < 1 = []
    | n == 1 = [(a, b)]
    | otherwise = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n -1 ) c b a

hanoiDefault :: Integer -> [Move]
hanoiDefault n = hanoi n "a" "b" "c"
