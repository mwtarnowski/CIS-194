{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2015: Homework 1

module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n | n <= 0    = []
              | otherwise = lastDigit n : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:y:ys) = x : 2*y : doubleEveryOther ys
doubleEveryOther ys = ys

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+).sum.toRevDigits) 0

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions
luhn :: Integer -> Bool
luhn = (0 ==) . lastDigit . sumDigits . doubleEveryOther . toRevDigits

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Exercise 7 -----------------------------------------

-- Frame–Stewart algorithm for 4 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = hanoi4 k a c b d ++ hanoi (n-k) a b d ++ hanoi4 k c b a d
                   where k = n - (round . sqrt . fromIntegral) (2*n + 1) + 1
