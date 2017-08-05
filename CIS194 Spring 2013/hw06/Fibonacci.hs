{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}
-- CIS 194 Spring 2013: Homework 6

module Fibonacci where

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

-- Exercise 5 -----------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream x xs) ys = Stream x (interleaveStreams ys xs)

ruler :: Stream Integer
ruler = start 0 where start n = interleaveStreams (streamRepeat n) (start (n+1))

-- Exercise 6 -----------------------------------------

instance Num (Stream Integer) where
    fromInteger n = Stream n $ streamRepeat 0
    negate = streamMap negate
    (+) (Stream p0 p') (Stream q0 q') = Stream (p0 + q0) (p' + q')
    (*) (Stream p0 p') q@(Stream q0 q') = Stream (p0 * q0) (streamMap (p0*) q' + p' * q)

instance Fractional (Stream Integer) where
    (/) p@(Stream p0 p') q@(Stream q0 q') = Stream (p0 `div` q0) (streamMap (`div` q0) (p' - p * q' / q))

xx :: Stream Integer
xx = Stream 0 $ Stream 1 $ streamRepeat 0
    
fibs3 :: Stream Integer
fibs3 = xx / (1 - xx - xx^(2::Integer))

-- Exercise 7 -----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    fromInteger n = Matrix n 0 0 n
    negate (Matrix a00 a01 a10 a11) = Matrix (-a00) (-a01) (-a10) (-a11)
    (+) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = 
        Matrix (a00 + b00) (a01 + b01) (a10 + b10) (a11 + b11)
    (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = 
        Matrix (a00 * b00 + a01 * b10) (a00 * b01 + a01 * b11) 
               (a10 * b00 + a11 * b10) (a10 * b01 + a11 * b11)

fib4 :: Integer -> Integer
fib4 n = fn where (Matrix _ fn _ _) = (Matrix 1 1 1 0)^n

fib4' :: Integer -> Integer
fib4' 0 = 0
fib4' n = fn where (Matrix fn _ _ _) = (Matrix 1 1 1 0)^(n-1)
