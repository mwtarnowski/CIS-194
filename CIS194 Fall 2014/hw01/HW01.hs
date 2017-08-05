{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Fall 2014: Homework 1

module HW01 where

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

-- Exercise 1 -----------------------------------------
-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = (`mod` 10)

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = (`div` 10)

-- Exercise 2 -----------------------------------------
-- Convert positive Integer to a list of its digits
toDigits :: Integer -> [Integer]
toDigits n | n <= 0    = []
           | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

-- Exercise 3 -----------------------------------------
-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleSeconds . reverse
                   where doubleSeconds (x:y:ys) = x : 2*y : doubleSeconds ys
                         doubleSeconds ys = ys

-- Exercise 4 -----------------------------------------
-- Sum over all digits of a list of Integers
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+).sum.toDigits) 0

-- Exercise 5 -----------------------------------------
-- Return True for a valid credit card number, False otherwise
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0


-- The Towers of Hanoi --------------------------------
type Peg = String
type Move = (Peg, Peg)

-- Exercise 6 -----------------------------------------
-- Return list of moves to move n discs from the first Peg to the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Exercise 7 -----------------------------------------
-- Frame–Stewart algorithm for 4 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = hanoi4 k a c b d ++ hanoi (n-k) a b d ++ hanoi4 k c b a d
                   where k = n - (round . sqrt . fromIntegral) (2*n + 1) + 1
