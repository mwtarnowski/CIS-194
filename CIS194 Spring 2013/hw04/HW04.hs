{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2013: Homework 4

module HW04 where

import Data.List

-- Exercise 1: Wholemeal programming ------------------

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs) | even x    = (x - 2) * fun1 xs
            | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even


fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate helper
        where helper n = if even n then (n `div` 2) else (3*n + 1)


-- Exercise 2: Folding with trees ---------------------

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ Leaf y right) = Node 1 (Node 0 Leaf x Leaf) y right
treeInsert x (Node _ left y Leaf)  = Node 1 left y (Node 0 Leaf x Leaf)
treeInsert x (Node _ left@(Node a _ _ _) y right@(Node b _ _ _))
    | a < b     = Node (1 + b) (treeInsert x left) y right
    | a > b     = Node (1 + a) left y (treeInsert x right)
    | otherwise = Node (1 + height right') left y right'
                  where right' = treeInsert x right
                        height tree = case tree of {Leaf -> -1; (Node n _ _ _) -> n}

treeInsert' :: a -> Tree a -> Tree a
treeInsert' x Leaf = Node 0 Leaf x Leaf
treeInsert' x (Node n left y right)
    | height left <= height right = let left'  = treeInsert' x left  
                                    in Node (1 + max (height left') (height right)) left' y right
    | height left >  height right = let right' = treeInsert' x right 
                                    in Node n left y right'
    where height tree = case tree of {Leaf -> -1; (Node k _ _ _) -> k}
treeInsert' _ tree = tree


foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf


-- Exercise 3: More folds -----------------------------

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base = foldr (flip f) base . reverse


-- Exercise 4: Finding primes -------------------------

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) ([1..n] \\ sieve)
                  where sieve = filter (<= n) [i + j + 2 * i * j | i <- [1..n], j <- [1..n]]

