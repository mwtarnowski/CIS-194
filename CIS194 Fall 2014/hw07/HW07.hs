{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- CIS 194 Fall 2014: Homework 7

module HW07 where

import System.Random

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
    show = show . take 20 . streamToList

-- Exercise 4 -----------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = start 0 where start n = interleaveStreams (streamRepeat n) (start (n+1))

-- Exercise 7 -----------------------------------------

randomList :: (Random a, RandomGen g) => g -> [a]
randomList = (\(x, g) -> x : randomList g) . random

randomList' :: (Random a, RandomGen g) => g -> [a]
randomList' = randoms

-- Exercise 8 -----------------------------------------

randomInts :: Int -> [Int]
randomInts = flip take $ randomList (mkStdGen 1)

-- Exercise 9 -----------------------------------------

minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 10 ----------------------------------------

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = case minMax xs of
                Nothing     -> Just (x, x)
                Just (y, z) -> Just (min x y, max x z)

-- Exercise 11 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer 
    deriving Show

instance Num Matrix where
    fromInteger n = Matrix n 0 0 n
    negate (Matrix a b c d) = Matrix (-a) (-b) (-c) (-d)
    (+) (Matrix a b c d) (Matrix a' b' c' d') = 
        Matrix (a + a') (b + b') (c + c') (d + d')
    (*) (Matrix a b c d) (Matrix a' b' c' d') = 
        Matrix (a * a' + b * c') (a * b' + b * d') 
               (c * a' + d * c') (c * b' + d * d')

fib4 :: Integer -> Integer
fib4 n = fn where (Matrix _ fn _ _) = (Matrix 1 1 1 0)^n

