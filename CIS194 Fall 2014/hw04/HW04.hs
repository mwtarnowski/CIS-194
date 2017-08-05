{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Fall 2014: Homework 4

module HW04 where

import Data.List
import Data.Char
import Data.Maybe
import BST

-- Exercise 1 -----------------------------------------
-- There is only one possible implementation:

-- ex1 = flip const
ex1 :: a -> b -> b
ex1 _ x = x

-- Exercise 2 -----------------------------------------
-- There are two possible implementations:

-- ex2 = const
ex2 :: a -> a -> a
ex2 x _ = x

-- ex2' = flip const
ex2' :: a -> a -> a
ex2' _ x = x

-- Exercise 3 -----------------------------------------
-- There is only one possible implementation:

-- ex3 = flip const
ex3 :: Int -> a -> a
ex3 _ x = x

-- Exercise 4 -----------------------------------------
-- Since |Bool| = 2 and |a -> a -> a| = 2 (see ex.2),
-- there are 2^2 = 4 possible implementations:

ex4 :: Bool -> a -> a -> a
ex4 False x _ = x
ex4 True  _ x = x

ex4' :: Bool -> a -> a -> a
ex4' False _ x = x
ex4' True  x _ = x

ex4'' :: Bool -> a -> a -> a
ex4'' _ x _ = x

ex4''' :: Bool -> a -> a -> a
ex4''' _ _ x = x

-- Exercise 5 -----------------------------------------
-- Since |Bool| = 2, there are 2^2 = 4 different 
-- functions of type Bool -> Bool:

-- ex5 = id
ex5 :: Bool -> Bool
ex5 p = p

-- ex5' = not
ex5' :: Bool -> Bool
ex5' p = not p

-- ex5'' = const True
ex5'' :: Bool -> Bool
ex5'' _ = True

-- ex5''' = const False
ex5''' :: Bool -> Bool
ex5''' _ = False

-- Exercise 6 -----------------------------------------
-- No implementation is possible, since given a function, 
-- there is no way to get a value of an arbitrary type.

ex6 :: (a -> a) -> a
ex6 = error "impossible"

-- Exercise 7 -----------------------------------------
-- There are infinitely many implementations.
-- One of them is just function application:

-- ex7 = ($)
ex7 :: (a -> a) -> a -> a
ex7 f x = f x

-- Since the first argument is an endomorphism, 
-- it can be applied multiple times:

ex7' :: (a -> a) -> a -> a
ex7' f x = iterate f x !! n
           where n = 100 {-arbitrary-}

-- Exercise 8 -----------------------------------------
-- There are infinitely many implementations.

ex8 :: [a] -> [a]
ex8 xs = concat $ replicate n xs 
         where n = 100 {-arbitrary-}

-- Exercise 9 -----------------------------------------
-- Among all possible implementations there is one that
-- seems most natural:

-- ex9 = map
ex9 :: (a -> b) -> [a] -> [b]
ex9 _ [] = []
ex9 f (x:xs) = f x : ex9 f xs

-- However there are infinitely many implementations, 
-- which arise from the previous exercise:

ex9' :: (a -> b) -> [a] -> [b]
ex9' f xs = ex8 $ ex9 f xs

-- Exercise 10 ----------------------------------------
-- No implementation is possible, since there is no way 
-- to get a value from Nothing.

ex10 :: Maybe a -> a
ex10 = error "impossible"

-- Exercise 11 ----------------------------------------
-- There are two possible implementations:

-- ex11 = Just
ex11 :: a -> Maybe a
ex11 x = Just x

-- ex11' = const Nothing
ex11' :: a -> Maybe a
ex11' _ = Nothing

-- Exercise 12 ----------------------------------------
-- There are two possible implementations:

-- ex12 = id
ex12 :: Maybe a -> Maybe a
ex12 mx = mx

-- ex12' = const Nothing
ex12' :: Maybe a -> Maybe a
ex12' _ = Nothing

-- Exercise 13 ----------------------------------------

insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node left y right) 
    | x `f` y == LT = Node (insertBST f x left) y right
    | otherwise     = Node left y (insertBST f x right)

-- Exercise 14 ----------------------------------------

allCaps :: [String] -> Bool
allCaps = all $ maybe False isUpper . listToMaybe

-- Exercise 15 ----------------------------------------

dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- Exercise 16 ----------------------------------------

firstLetters :: [String] -> [Char]
firstLetters = mapMaybe listToMaybe

-- Exercise 17 ----------------------------------------

asList :: [String] -> String
asList =  ("[" ++) . (++ "]") . intercalate ","
