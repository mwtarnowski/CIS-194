{-# OPTIONS_GHC -Wall #-}
-- CIS 194 Spring 2013: Homework 3

module Golf where
import Data.List


-- Exercise 1: Hopscotch ------------------------------
-- Return a list of lists, such that the n-th list in the output 
-- contains every n-th element from the input list.

skips :: [a] -> [[a]]
skips xs = [[x | (x,k) <- zip xs [1..], mod k n == 0] | n <- [1..length xs]]


-- Exercise 2: Local maxima ---------------------------
-- Find all the local maxima in the input list and return them in order.

localMaxima :: [Integer] -> [Integer]
localMaxima (x:xs@(y:z:_)) = (if x < y && y > z then [y] else []) ++ localMaxima xs
localMaxima _ = []


-- Exercise 3: Histogram ------------------------------
-- Take as input a list of Integers between 0 and 9 (inclusive),
-- and return a vertical histogram showing how many of each number
-- were in the input list.

histogram :: [Integer] -> String
histogram xs = unlines . transpose $ map hlp [0..9]
               where ys = [length (filter (==n) xs) | n <- [0..9]]
                     hlp n = let l = ys !! n; w = maximum ys in
                             replicate (w-l) ' ' ++ replicate l '*' ++ "=" ++ show n

printHistogram :: [Integer] -> IO()
printHistogram = putStr . histogram
