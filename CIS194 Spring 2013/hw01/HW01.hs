{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2013: Homework 1

module HW01 where

-- Validating Credit Card Numbers ---------------------

-- Exercise 1 -----------------------------------------
-- Convert positive Integers to a reversed list of their digits.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n | n > 0  = (n `mod` 10) : toDigitsRev (n `div` 10)
toDigitsRev _ = []

-- Convert positive Integers to a list of their digits.
toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Exercise 2 -----------------------------------------
-- Double every other number beginning from the right
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleSeconds . reverse
                   where doubleSeconds (x:y:ys) = x : 2*y : doubleSeconds ys
                         doubleSeconds ys = ys

-- Exercise 3 -----------------------------------------
-- Sum over all digits of a list of Integers
sumDigits :: [Integer] -> Integer
sumDigits = foldr ((+).sum.toDigits) 0

-- Exercise 4 -----------------------------------------
-- Return True for a valid credit card number, False otherwise
validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0


-- The Towers of Hanoi --------------------------------
type Peg = String
type Move = (Peg, Peg)

-- Exercise 5 -----------------------------------------
-- Return list of moves to move n discs from the first Peg to the second
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- Exercise 6 -----------------------------------------
-- Frame–Stewart algorithm for 4 pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
hanoi4 n a b c d = hanoi4 k a c b d ++ hanoi (n-k) a b d ++ hanoi4 k c b a d
                   where k = n - (round . sqrt . fromIntegral) (2*n + 1) + 1
