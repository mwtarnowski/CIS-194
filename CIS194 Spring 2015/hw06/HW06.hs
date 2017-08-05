{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
-- CIS 194 Spring 2015: Homework 6

module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s) ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons x xs) = x : streamToList xs

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons x xs) ys = Cons x (sInterleave ys xs)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons x xs) = x : sTake (n-1) xs

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = start 0 where start n = sInterleave (sRepeat n) (start (n+1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate (\n -> (1103515245 * n + 12345) `mod` 2147483648)

-- Exercise 8 -----------------------------------------

minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = case minMax xs of
                Nothing     -> Just (x, x)
                Just (y, z) -> Just (min x y, max x z)

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer deriving Show

instance Num Matrix where
    fromInteger n = Matrix n 0 0 n
    negate (Matrix a00 a01 a10 a11) = Matrix (-a00) (-a01) (-a10) (-a11)
    (+) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = 
        Matrix (a00 + b00) (a01 + b01) (a10 + b10) (a11 + b11)
    (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) = 
        Matrix (a00 * b00 + a01 * b10) (a00 * b01 + a01 * b11) 
               (a10 * b00 + a11 * b10) (a10 * b01 + a11 * b11)

-- The implementation of fastFib follows the instruction. 
-- However it is inconsistent with the function fib1.
-- fastFib 0 = 0; fastFib n = fib1 (n-1) for n > 0
fastFib :: Integer -> Integer
fastFib n = fn where (Matrix _ fn _ _) = (Matrix 1 1 1 0)^n
